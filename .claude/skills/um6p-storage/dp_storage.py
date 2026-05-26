#!/usr/bin/env python3
"""dp_storage — command-line client for the UM6P Data Playground file storage.

Dependency-free (Python 3.8+ standard library only). Wraps the
``/api/v1`` storage API so a Claude skill — or a human — can move files
to and from the lab's centralized object store.

Authentication
--------------
All commands except ``login`` need a bearer token. Provide it via either:
  * the ``DP_TOKEN`` environment variable, or
  * a token file at ``~/.um6p_storage_token`` (written by ``login``).

The server base URL defaults to the production playground; override with
the ``DP_BASE_URL`` environment variable.

Usage
-----
  dp_storage.py login [--email EMAIL] [--password PASS] [--name LABEL]
  dp_storage.py whoami
  dp_storage.py ls [PREFIX] [--recursive]
  dp_storage.py get  REMOTE_KEY [LOCAL_PATH]
  dp_storage.py put  LOCAL_PATH [REMOTE_DIR] [--name NAME]
  dp_storage.py mkdir REMOTE_PATH
  dp_storage.py rm   REMOTE_KEY

Paths
-----
A "prefix"/"dir" given without a leading ``u<id>/`` is interpreted
relative to your own namespace. For example ``data/raw`` means
``u<your-id>/data/raw/``. To browse another user's files, pass their
absolute prefix (e.g. ``u7/``) — you can read anyone's files but only
write within your own namespace.
"""

import argparse
import getpass
import json
import mimetypes
import os
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
import uuid

DEFAULT_BASE_URL = "https://ecu-data-playground.ngrok.app"
TOKEN_FILE = os.path.expanduser("~/.um6p_storage_token")


# ── configuration ────────────────────────────────────────────

def base_url():
    return os.environ.get("DP_BASE_URL", DEFAULT_BASE_URL).rstrip("/")


def load_token():
    tok = os.environ.get("DP_TOKEN", "").strip()
    if tok:
        return tok
    if os.path.isfile(TOKEN_FILE):
        with open(TOKEN_FILE) as fh:
            return fh.read().strip()
    return None


def require_token():
    tok = load_token()
    if not tok:
        die("No API token found. Run `dp_storage.py login` first, "
            "or set the DP_TOKEN environment variable.")
    return tok


def die(msg, code=1):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(code)


# ── HTTP helpers ─────────────────────────────────────────────

def _request(method, path, token=None, params=None, data=None,
             headers=None, expect_json=True):
    url = base_url() + path
    if params:
        url += "?" + urllib.parse.urlencode(params)
    hdrs = dict(headers or {})
    if token:
        hdrs["Authorization"] = "Bearer " + token
    req = urllib.request.Request(url, data=data, method=method, headers=hdrs)
    try:
        resp = urllib.request.urlopen(req)
    except urllib.error.HTTPError as e:
        body = e.read()
        try:
            msg = json.loads(body).get("error", body.decode("utf-8", "replace"))
        except Exception:
            msg = body.decode("utf-8", "replace")
        die(f"{method} {path} -> HTTP {e.code}: {msg}")
    except urllib.error.URLError as e:
        die(f"could not reach {url}: {e.reason}")
    if expect_json:
        return json.loads(resp.read())
    return resp


# ── commands ─────────────────────────────────────────────────

def cmd_login(args):
    email = args.email or input("Email: ").strip()
    password = args.password or getpass.getpass("Password: ")
    payload = json.dumps({
        "email": email, "password": password, "name": args.name,
    }).encode("utf-8")
    data = _request("POST", "/api/v1/auth/token", data=payload,
                    headers={"Content-Type": "application/json"})
    with open(TOKEN_FILE, "w") as fh:
        fh.write(data["token"])
    os.chmod(TOKEN_FILE, 0o600)
    print(f"Logged in as {data['user']['name']} <{data['user']['email']}>")
    print(f"Namespace: {data['namespace']}")
    print(f"Token saved to {TOKEN_FILE} (expires {data.get('expires_at')})")


def cmd_whoami(args):
    data = _request("GET", "/api/v1/whoami", token=require_token())
    print(json.dumps(data, indent=2))


def cmd_ls(args):
    params = {"prefix": args.prefix or ""}
    if args.recursive:
        params["recursive"] = "1"
    data = _request("GET", "/api/v1/files", token=require_token(), params=params)
    print(f"# {data['prefix']}")
    for folder in data.get("folders", []):
        print(f"  [dir]  {folder}")
    for f in data.get("files", []):
        size = f.get("size", 0)
        print(f"  {size:>12}  {f['key']}")
    if not data.get("folders") and not data.get("files"):
        print("  (empty)")


def cmd_get(args):
    token = require_token()
    data = _request("GET", "/api/v1/files/download", token=token,
                    params={"key": args.key})
    dest = args.dest or os.path.basename(args.key.rstrip("/"))
    if os.path.isdir(dest):
        dest = os.path.join(dest, os.path.basename(args.key.rstrip("/")))
    # The presigned URL needs no auth header.
    with urllib.request.urlopen(data["url"]) as resp, open(dest, "wb") as out:
        while True:
            chunk = resp.read(1 << 20)
            if not chunk:
                break
            out.write(chunk)
    print(f"Downloaded {args.key} -> {dest} ({data.get('size')} bytes)")


def cmd_put(args):
    token = require_token()
    if not os.path.isfile(args.local):
        die(f"no such file: {args.local}")
    filename = args.name or os.path.basename(args.local)
    ctype = mimetypes.guess_type(filename)[0] or "application/octet-stream"

    # Build a streaming multipart/form-data body in a temp file so large
    # uploads are not buffered entirely in memory.
    boundary = uuid.uuid4().hex
    body = tempfile.TemporaryFile()

    def w(s):
        body.write(s if isinstance(s, bytes) else s.encode("utf-8"))

    if args.remote_dir:
        w(f"--{boundary}\r\n")
        w('Content-Disposition: form-data; name="dir"\r\n\r\n')
        w(args.remote_dir + "\r\n")
    w(f"--{boundary}\r\n")
    w(f'Content-Disposition: form-data; name="file"; filename="{filename}"\r\n')
    w(f"Content-Type: {ctype}\r\n\r\n")
    with open(args.local, "rb") as fh:
        while True:
            chunk = fh.read(1 << 20)
            if not chunk:
                break
            body.write(chunk)
    w(f"\r\n--{boundary}--\r\n")

    length = body.tell()
    body.seek(0)
    headers = {
        "Authorization": "Bearer " + token,
        "Content-Type": f"multipart/form-data; boundary={boundary}",
        "Content-Length": str(length),
    }
    req = urllib.request.Request(
        base_url() + "/api/v1/files/upload", data=body, method="POST",
        headers=headers,
    )
    try:
        resp = urllib.request.urlopen(req)
        result = json.loads(resp.read())
    except urllib.error.HTTPError as e:
        try:
            msg = json.loads(e.read()).get("error")
        except Exception:
            msg = f"HTTP {e.code}"
        die(f"upload failed: {msg}")
    finally:
        body.close()
    print(f"Uploaded {args.local} -> {result['key']} ({result.get('size')} bytes)")


def cmd_mkdir(args):
    payload = json.dumps({"path": args.path}).encode("utf-8")
    data = _request("POST", "/api/v1/files/folder", token=require_token(),
                    data=payload, headers={"Content-Type": "application/json"})
    print(f"Created folder {data['prefix']}")


def cmd_rm(args):
    data = _request("DELETE", "/api/v1/files", token=require_token(),
                    params={"key": args.key})
    print(data.get("message", "Deleted."))


# ── argument parsing ─────────────────────────────────────────

def build_parser():
    p = argparse.ArgumentParser(
        prog="dp_storage.py",
        description="UM6P Data Playground file-storage client.",
    )
    sub = p.add_subparsers(dest="command", required=True)

    sp = sub.add_parser("login", help="exchange email+password for a token")
    sp.add_argument("--email")
    sp.add_argument("--password")
    sp.add_argument("--name", default="Claude skill", help="label for the token")
    sp.set_defaults(func=cmd_login)

    sp = sub.add_parser("whoami", help="show the authenticated account")
    sp.set_defaults(func=cmd_whoami)

    sp = sub.add_parser("ls", help="list files under a prefix")
    sp.add_argument("prefix", nargs="?", default="")
    sp.add_argument("--recursive", action="store_true")
    sp.set_defaults(func=cmd_ls)

    sp = sub.add_parser("get", help="download a file")
    sp.add_argument("key")
    sp.add_argument("dest", nargs="?")
    sp.set_defaults(func=cmd_get)

    sp = sub.add_parser("put", help="upload a file into your namespace")
    sp.add_argument("local")
    sp.add_argument("remote_dir", nargs="?", default="",
                    help="destination folder, relative to your namespace")
    sp.add_argument("--name", help="override the stored filename")
    sp.set_defaults(func=cmd_put)

    sp = sub.add_parser("mkdir", help="create an (empty) folder")
    sp.add_argument("path")
    sp.set_defaults(func=cmd_mkdir)

    sp = sub.add_parser("rm", help="delete a file or folder (folder keys end with /)")
    sp.add_argument("key")
    sp.set_defaults(func=cmd_rm)

    return p


def main():
    args = build_parser().parse_args()
    args.func(args)


if __name__ == "__main__":
    main()

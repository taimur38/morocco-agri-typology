# Data Playground Storage API (`/api/v1`)

Base URL (production): `https://ecu-data-playground.ngrok.app`

All endpoints return JSON. Every endpoint except `POST /auth/token`
requires a bearer token:

```
Authorization: Bearer dpk_xxxxxxxxxxxxxxxxxxxxxxxx
```

A token may also be passed as the `?api_key=` query parameter.

## Authentication

### `POST /api/v1/auth/token`

Exchange Data Playground credentials for a bearer token. This is the only
unauthenticated endpoint.

Request body (JSON):

| field      | required | description                                  |
|------------|----------|----------------------------------------------|
| `email`    | yes      | Data Playground account email                |
| `password` | yes      | account password                             |
| `name`     | no       | label for the token (default `Claude skill`) |
| `days`     | no       | validity in days, 1–365 (default 90)          |

Response:

```json
{
  "token": "dpk_....",
  "token_type": "Bearer",
  "expires_at": "2026-08-16T...",
  "user": {"id": 5, "name": "username", "email": "username@..."},
  "namespace": "u5/"
}
```

The raw `token` is shown **once** — store it. Tokens can be revoked from
the website (**Files → API Tokens**).

### `GET /api/v1/whoami`

Returns the account and token bound to the request.

## Files

Path rules: a `prefix`/`dir` without a leading `u<id>/` is resolved
relative to the caller's namespace. Absolute `u<id>/...` prefixes may be
**read** by anyone; **writes** are only allowed within the caller's own
namespace. `..`, absolute paths and null bytes are rejected.

### `GET /api/v1/files?prefix=&recursive=`

List files. With `recursive=1`, returns every file under the prefix and no
folders; otherwise returns one level (`folders` + `files`).

```json
{
  "prefix": "u5/data/raw/",
  "recursive": false,
  "folders": ["u5/data/raw/2024/"],
  "files": [
    {"key": "u5/data/raw/index.csv", "name": "index.csv",
     "size": 20481, "last_modified": "2026-05-18T..."}
  ]
}
```

### `GET /api/v1/files/download?key=&redirect=`

Returns a short-lived presigned download URL (valid 1 hour) for `key`:

```json
{"key": "u5/data/raw/index.csv", "size": 20481,
 "content_type": "text/csv", "url": "https://...", "expires_in": 3600}
```

The `url` requires no auth header — fetch it directly. Pass `redirect=1`
to receive a `302` to the URL instead of JSON.

### `POST /api/v1/files/upload`

`multipart/form-data` upload into the caller's namespace.

| part / param | required | description                                       |
|--------------|----------|---------------------------------------------------|
| `file`       | yes      | the file (multipart file part)                    |
| `dir`        | no       | destination folder, relative to your namespace    |
| `name`       | no       | override the stored filename                      |

Response: `{"message": "Uploaded.", "key": "u5/data/raw/index.csv", "size": 20481}`

```bash
curl -H "Authorization: Bearer $DP_TOKEN" \
     -F "dir=data/raw" -F "file=@index.csv" \
     https://ecu-data-playground.ngrok.app/api/v1/files/upload
```

### `POST /api/v1/files/folder`

Create an empty folder. Body: `{"path": "data/processed"}`.

### `DELETE /api/v1/files?key=`

Delete a file, or a whole folder if `key` ends with `/`. Only keys inside
the caller's own namespace may be deleted (`403` otherwise).

## Errors

Non-2xx responses carry `{"error": "..."}`. Common codes: `400` invalid
input/path, `401` missing/expired token, `403` writing outside your
namespace or inactive account, `404` file not found, `503` storage not
configured on the server.

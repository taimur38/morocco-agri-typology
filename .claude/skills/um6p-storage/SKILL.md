---
name: um6p-storage
description: >-
  Upload, download, list, or sync data files with the UM6P Data Playground
  centralized file storage. Use this whenever the user wants to fetch raw
  data the team has shared, push a file/dataset to the shared store, list
  what is available under a collection (folder), or otherwise move files
  to/from the lab's storage instead of committing them to git.
---

# UM6P Data Playground — File Storage

The lab keeps **raw data and other large files outside of git**, in a
centralized object store reachable through the Data Playground website.
Code goes in GitHub; data goes here. This skill moves files in and out of
that store.

Everything is done through `dp_storage.py` (bundled with this skill — pure
Python 3 standard library, no `pip install` needed).

## Namespaces and collections

- Every file lives under your personal namespace: `u<your-user-id>/...`.
- Within your namespace you organize files into **collections** — ordinary
  folders, e.g. `data/raw/`, `data/processed/`.
- **You can read (list + download) any user's files**; you can only
  **upload/delete within your own namespace**.
- Paths passed to `ls`/`put` without a leading `u<id>/` are relative to
  *your* namespace. To read a teammate's files, use their absolute prefix
  (e.g. `u7/data/raw/`).

## One-time setup

1. The user needs a Data Playground account (the same login as the
   website). Authenticate once to mint an API token:

   ```bash
   python dp_storage.py login --email USER_EMAIL
   ```

   This prompts for the password and saves a token to
   `~/.um6p_storage_token` (mode 600). The token is bound to the account,
   so all activity is traceable.

   Alternatively, the user can create a token from the website
   (**Files → API Tokens → Create Token**) and export it:

   ```bash
   export DP_TOKEN="dpk_...."
   ```

2. Confirm it works:

   ```bash
   python dp_storage.py whoami
   ```

If the server is not the default, set `DP_BASE_URL` (default is
`https://ecu-data-playground.ngrok.app`).

## Common operations

List a collection:

```bash
python dp_storage.py ls data/raw
python dp_storage.py ls data/raw --recursive
python dp_storage.py ls u7/            # browse teammate u7's files
```

Download a file (downloads via a short-lived presigned URL):

```bash
python dp_storage.py get u7/data/raw/agri_index_2024.csv ./local/
```

Upload a file into one of your collections:

```bash
python dp_storage.py put ./agri_index_2024.csv data/raw
```

Create an empty collection, or delete files:

```bash
python dp_storage.py mkdir data/processed
python dp_storage.py rm data/raw/old.csv
python dp_storage.py rm data/raw/            # delete a whole folder
```

## How to use this skill

- When the user asks to **get the latest data** / **pull the raw data**:
  `ls` the relevant collection to see what exists, then `get` the file(s).
- When the user has produced an output file to **share with the team**:
  `put` it into an appropriate collection under their namespace and tell
  them the resulting key so teammates can `get` it.
- Prefer descriptive, dated filenames and stable collection names
  (`data/raw/`, `data/processed/`) so the store stays organized.
- **Never** commit large data files to git — that is the whole reason this
  store exists. Keep code in GitHub, data here.
- Report the exact key (e.g. `u5/data/raw/file.csv`) after any upload so
  it can be referenced later.

## Direct API access

If scripting without the CLI, every command maps to a documented JSON
endpoint under `/api/v1` — see `API.md` in this skill folder. Auth is a
bearer token: `Authorization: Bearer <token>`.

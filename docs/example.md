---
hide:
  - navigation
---

# Examples

```bash title="Download from remote to local - filter by date"
hsftp -c conf.yaml -d \
    --transfer-from /path/to/remote/folder \
    --transfer-to /path/to/local/folder \
    --from-date "2024-06-14 12:15 PDT"
```

```bash title="Upload from local to remote - filter by extensions"
hsftp -c conf.yaml -u \
    --transfer-from /path/to/local/folder \
    --transfer-to /path/to/remote/folder \
    -e xml -e Xml
```

```bash title="Upload from local to remote - archive files locally after upload"
hsftp -c conf.yaml -u \
    --transfer-from /path/to/local/folder \
    --transfer-to /path/to/remote/folder \
    --archive-to /path/to/local/archive/folder
```

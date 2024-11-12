---
hide:
  - navigation
---

# Getting Started 

**Hsftp** is a SFTP client tool for secure file transfer operations.

## Usage

``` { .bash .no-copy }
Hsftp 1.3.1. Usage: hsftp OPTION

hsftp [OPTIONS] [ITEM]

Common flags:
  -c --conf=FILE          Load conf from file
     --from-date=DATE     Filter files by date (YYYY-MM-DD HH:MM UTC|PST|...)
  -e --extensions=ITEM    Filter files by extensions
  -u --up                 upload
  -d --down               download
     --transfer-from=DIR  Folder to transfer from
     --transfer-to=ITEM   Folder to transfer to
     --archive-to=DIR     Folder to archive to after upload

Miscellaneous:
     --verbose=INT        Verbose level: 1, 2 or 3
  -n --dry-run            Do a dry-run ("No-op") transfer.
  -? --help               Display help message
  -V --version            Print version information
     --numeric-version    Print just the version number
```

## Configuration

``` { .yaml .no-select title="conf.yaml" linenums="1" }
remote:
        hostname: sftp.domain.com
        port: 22
        username: username
        password: password
        known_hosts: /home/user/.ssh/known_hosts
```

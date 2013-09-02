EGit SSHD
=========

egit_sshd is an ssh daemon for git repos.

Getting Started
---------------

```
$ cp config/example.sys.config config/sys.config
```

Paste your hosts private rsa and dsa keys into the `config/sys.config` file and configure the location of your git repos.

```
$ make rel
$ ./_rel/bin/egit_sshd console
```

Todo
----

* Currently `egit_sshd_key_api` simply returns true for any user and key. It needs to provide a pluggable interface to validate a user and his key.
* Better command filtering. The callback simply matches on `git-receive-pack` and `git-upload-pack` right now.

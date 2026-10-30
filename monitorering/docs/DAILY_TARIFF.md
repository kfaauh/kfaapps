# Daily tariff collection and app publication

The strict job reuses the legacy collector's login, ASP.NET helpers, Excel parser and RDS preparation. It replaces its permissive group loop in memory; no embedded password is used and no existing app source is changed. It runs as the operator, not root. Original source files remain unchanged.

## First installation: user-operated on the Ubuntu server

From the Monitorering checkout and the normal AU login:

```bash
Rscript monitorering/scripts/install.R
python3 monitorering/admin/setup_tariff_job.py
sudo systemctl start monitorering-priser.service
sudo journalctl -u monitorering-priser.service --no-pager -n 100
```

Setup asks for sudo authorization and the Medicine Prices email/password (hidden password prompt). It stores settings in root-owned `/etc/monitorering-priser.env`, mode 0600, outside git. Systemd reads it before starting the unprivileged R process. The configuration is never printed or written to the journal. An existing configuration is retained rather than overwritten. To rotate credentials, edit that file with `sudoedit /etc/monitorering-priser.env`; it uses systemd EnvironmentFile syntax, not shell expansion. Never paste the file into chat. Setup does not start or enable the timer.

Setup grants the AU user ownership of **only** `/srv/shiny-server/kfaapps/lmgrupper2/data` (group shiny, setgid 2750) and the app's restart marker (0640), so the process can atomically replace data without write access to app code. The new RDS is 0640 and inherits the shiny group. Code and R libraries under the AU home are read-only within the service; only the runtime, target app data directory and restart marker are writable. Paths are explicit and point to the existing home database. The script assumes the catalogue and installed app locations established in this project; it refuses missing files.

The service writes all downloaded/prepared files into a unique run directory under `~/monitorering-data/`. It never writes directly into the app data directory during collection. It takes a single-job lock and uses the existing database writer lock for import. A full database+archive backup is taken if none exists in `runtime/backups` or the newest backup is at least seven days old. No backups or source captures are automatically deleted; monitor disk space.

## Acceptance gate and activation

Only after a successful **manual service run** and a working app:

```bash
python3 monitorering/admin/activate_tariff_job.py
```

Activation checks both systemd success and a publication receipt from the last 24 hours. It backs up root's crontab under the user's private `monitorering-data/admin-backups/`. It removes exactly the known `&& /usr/bin/Rscript fetch_data2.R ... >> ... 2>&1` invocation and the legacy Clean_data2.rds ownership argument; unknown layouts or multiple invocations abort without editing. A repeated activation is idempotent when the legacy command is absent. All unrelated cron commands are retained. In particular, the separately broken `data_management.R` job is NOT fixed or removed; it may still report errors but no longer blocks this independent tariff service. If enabling the timer fails, the original crontab is restored.

The timer runs daily at 03:15 Europe/Copenhagen with up to two minutes randomized delay, including catch-up after downtime. Login runs through the user's normal authorized subscription. A browser/PowerBI data collection is **not** scheduled. The separate consumption inbox importer remains optional.

## Processing and evidence

- Fetch the current live group dropdown, require all previously supported groups, include new recognized groups, and reject an unexpected catalogue.
- Sequential requests, at least two seconds between groups, bounded HTTP timeout/retries. Every search response must confirm the selected group and the same current source nominal price period.
- A group is empty only with an explicit zero-result statement in its search-result container and no contradictory product rows/links. HTML export alone is never evidence of zero. The seven groups manually checked on 2026-10-30 are NOT permanently excluded or hardcoded as empty.
- Every nonempty group must yield a valid Excel export. Any ambiguous/error/login/malformed response fails the whole capture before app publication. Limited result-container diagnostic text is saved for ambiguous HTML; raw HTML, viewstate, cookies and passwords are not archived or logged.
- Match raw/prepared rows, six-digit unique product keys, required app fields, package prices and numeric validity. Default minimum 10,000 rows and maximum 20% decline from the deployed app; adjust `MONITOR_MIN_ROWS`/`MONITOR_MAX_ROW_DROP` in the root config after reviewing legitimate source changes.
- Archive the exact raw workbook and metadata with per-group exported/confirmed-empty evidence in DuckDB. `complete=true` here means all source-dropdown groups were processed; it is NOT independent proof of national population coverage.
- Nominal price period comes from source banners. Effective daily validity is not invented: snapshots remain `observed_only`. Legacy inferred DDD is still not imported as a verified denominator.
- Back up the previous app RDS, copy/checksum a temporary file on the same filesystem, atomically replace the app RDS and touch only that app's `restart.txt`. Restart failure restores the previous file. An imported, valid database capture can exist even if subsequent app publication fails; the service then exits nonzero and does not write a success receipt.

## Routine checks and recovery

```bash
sudo systemctl list-timers monitorering-priser.timer --no-pager
sudo systemctl status monitorering-priser.service --no-pager
sudo journalctl -u monitorering-priser.service --since yesterday --no-pager
cat "$HOME/monitorering-data/last-tariff-success.json"
```

Successful oneshot services normally show `inactive (dead)` after exit; check `Result=success`, the receipt and the journal. Failed updates remain visible in systemd/journal; no email/Slack alert is configured. Follow up failures and disk space. To stop scheduling: `sudo systemctl disable --now monitorering-priser.timer` (this does not undo an already completed publication). Avoid changing checkout branches while a service run is active. After changing code, rerun local/offline tests and one manual service run before relying on the schedule.

Source HTML may change. If the zero-result wording/container does not match, the job deliberately rejects publication and leaves the current app data in place. Inspect its private diagnostic and update the classifier using a real sanitized source example; do not turn all HTML into zero.

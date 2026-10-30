#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
RSCRIPT=${RSCRIPT:-Rscript}
test_dir=$(mktemp -d)
trap 'rm -rf "$test_dir"' EXIT
export MONITOR_RUNTIME="$test_dir/runtime"
run() { "$RSCRIPT" monitorering/scripts/monitor.R "$@"; }
run init
run import-consumption monitorering/tests/fixtures/monitorering_forbrug_N06_2025-01.csv monitorering/tests/fixtures/monitorering_manifest_N06_2025-01.json
run import-consumption monitorering/tests/fixtures/monitorering_forbrug_N06_2025-01.csv monitorering/tests/fixtures/monitorering_manifest_N06_2025-01.json
run validate
run coverage N06 2025-01 2025-02
run export v_consumption "$test_dir/export.csv"
run backup "$test_dir/backups"
backup_dir=$(find "$test_dir/backups" -mindepth 1 -maxdepth 1 -type d)
MONITOR_RUNTIME="$test_dir/restored" run restore "$backup_dir"
MONITOR_RUNTIME="$test_dir/restored" run validate
# Real second-process lock exclusion. The lock file is persistent; do not delete it.
"$RSCRIPT" -e 'a<-commandArgs(TRUE);l<-filelock::lock(a[1],timeout=0);writeLines("ready",a[2]);Sys.sleep(8);filelock::unlock(l)' \
  "$MONITOR_RUNTIME/monitorering.duckdb.writer.lock" "$test_dir/ready" &
lock_pid=$!
for i in $(seq 1 100); do
  if test -f "$test_dir/ready"; then break; fi
  sleep 0.1
done
test -f "$test_dir/ready"
if run init; then echo 'FAIL: simultaneous writer was accepted'; kill "$lock_pid"; exit 1; fi
wait "$lock_pid"
# Failed validation yields nonzero exit and cannot corrupt the successful import.
cp monitorering/tests/fixtures/monitorering_manifest_N06_2025-01.json "$test_dir/bad.json"
sed -i 's/"row_count": 1143/"row_count": 1144/' "$test_dir/bad.json"
if run import-consumption monitorering/tests/fixtures/monitorering_forbrug_N06_2025-01.csv "$test_dir/bad.json"; then
 echo 'FAIL: invalid manifest was accepted'; exit 1
fi
run validate
node monitorering/tests/write_v2.cjs "$test_dir/v2"
run import-inbox "$test_dir/v2"
run import-inbox "$test_dir/v2"
run validate
echo 'PASS: CLI init/import/idempotence/coverage/export/backup/restore/lock/rejection'

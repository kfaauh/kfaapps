#!/usr/bin/env python3
"""Enable only after a successful service run; remove only the legacy fetch_data2 invocation."""
import datetime, json, os, pathlib, re, subprocess

def detach_legacy(cron):
    lines=cron.splitlines(keepends=True)
    candidates=[i for i,line in enumerate(lines) if not line.lstrip().startswith('#') and 'fetch_data2.R' in line]
    if not candidates:
        return cron,False
    if len(candidates)!=1:
        raise ValueError('Multiple legacy price jobs found; no cron changes made')
    i=candidates[0]
    pattern=r'\s*&&\s*/usr/bin/Rscript\s+fetch_data2\.R\s+/srv/shiny-server/kfaapps/lmgrupper2/data\s+"Alle lægemidler\.xlsx"\s*>>\s*/var/log/kfaapps_data_update\.log\s+2>&1'
    line,n=re.subn(pattern,'',lines[i])
    if n!=1 or 'fetch_data2.R' in line:
        raise ValueError('Unexpected legacy command layout; no cron changes made')
    line=line.replace(' /srv/shiny-server/kfaapps/lmgrupper2/data/Clean_data2.rds','')
    line=line.replace(' /srv/shiny-server/kfaapps/lmgrupper2/data/clean_data2.rds','')
    lines[i]=line
    return ''.join(lines),True

def main():
    if os.getuid()==0:
        raise SystemExit('Run this as your normal AU user, without sudo.')
    show=subprocess.check_output(['sudo','systemctl','show','monitorering-priser.service','--property=Result,ExecMainStatus,ExecMainStartTimestamp'],text=True)
    values=dict(line.split('=',1) for line in show.splitlines() if '=' in line)
    if values.get('Result')!='success' or values.get('ExecMainStatus')!='0' or not values.get('ExecMainStartTimestamp'):
        raise SystemExit('A successful manual service run is required before activation.')
    runtime=pathlib.Path.home()/'monitorering-data'
    receipt=json.loads((runtime/'last-tariff-success.json').read_text())
    if receipt.get('status')!='published':
        raise SystemExit('Publication receipt missing')
    captured=datetime.datetime.fromisoformat(receipt['time'].replace('Z','+00:00'))
    if (datetime.datetime.now(datetime.timezone.utc)-captured).total_seconds()>86400:
        raise SystemExit('Run the service successfully today before activation')
    result=subprocess.run(['sudo','crontab','-l'],text=True,capture_output=True)
    if result.returncode!=0:
        raise SystemExit('Cannot read the existing root crontab; no changes made')
    old=result.stdout
    new,changed=detach_legacy(old)
    if changed:
        folder=runtime/'admin-backups';folder.mkdir(mode=0o700,exist_ok=True)
        backup=folder/('root-crontab-'+datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')+'.txt')
        backup.write_text(old);backup.chmod(0o600)
        current=subprocess.check_output(['sudo','crontab','-l'],text=True)
        if current!=old:
            raise SystemExit('Root crontab changed during setup; no changes made. Run activation again.')
        subprocess.run(['sudo','crontab','-'],input=new,text=True,check=True)
        print('Legacy fetch_data2 detached. Original root crontab backup:',backup)
    try:
        subprocess.run(['sudo','systemctl','enable','--now','monitorering-priser.timer'],check=True)
    except subprocess.CalledProcessError:
        if changed:
            subprocess.run(['sudo','crontab','-'],input=old,text=True,check=True)
        raise
    print('Daily timer enabled. All other cron commands are retained, including the separately unresolved data_management.R job.')
    subprocess.run(['sudo','systemctl','list-timers','monitorering-priser.timer','--no-pager'],check=True)

if __name__=='__main__':
    main()

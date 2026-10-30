#!/usr/bin/env python3
"""Interactive installation, invoked by the server operator. Does not start/enable jobs."""
import getpass, grp, os, pathlib, pwd, shutil, subprocess, tempfile

def sudo(*args, **kwargs):
    return subprocess.run(['sudo', *args],check=True,**kwargs)

def envquote(value):
    if '\n' in value or '\r' in value or '\0' in value:
        raise ValueError('Newlines/NUL are not supported in configuration values')
    return '"'+value.replace('\\','\\\\').replace('"','\\"')+'"'

def main():
    account=pwd.getpwuid(os.getuid())
    if os.getuid()==0:
        raise SystemExit('Run python3 setup_tariff_job.py as your normal AU user, without sudo.')
    code=pathlib.Path(__file__).resolve().parents[2]
    home=pathlib.Path(account.pw_dir)
    runtime=home/'monitorering-data'
    app=pathlib.Path('/srv/shiny-server/kfaapps/lmgrupper2')
    catalog=pathlib.Path('/srv/shiny-server/kfaapps/data-scripts/Alle lægemidler.xlsx')
    if not (runtime/'monitorering.duckdb').exists() or not (app/'data/clean_data2.rds').exists() or not catalog.exists():
        raise SystemExit('Existing Monitorering database, app RDS and catalogue must exist before setup.')
    rscript=shutil.which('Rscript')
    if not rscript:
        raise SystemExit('Rscript not found')
    config=pathlib.Path('/etc/monitorering-priser.env')
    print('Først godkender du installationen med din AU/server-adgangskode, hvis sudo spørger.',flush=True)
    sudo('-v')
    if subprocess.run(['systemctl','is-enabled','--quiet','monitorering-priser.timer'],capture_output=True).returncode==0:
        raise SystemExit('Disable the existing timer before reconfiguring: sudo systemctl disable --now monitorering-priser.timer')
    active=subprocess.run(['systemctl','show','monitorering-priser.service','--property=ActiveState','--value'],capture_output=True,text=True)
    if active.stdout.strip() in ('active','activating','reloading'):
        raise SystemExit('Wait for the existing service run to finish before setup.')
    if not config.exists():
        print('Nu bruges dit særskilte login til erhverv.medicinpriser.dk.',flush=True)
        email=input('E-mail til erhverv.medicinpriser.dk: ').strip()
        password=getpass.getpass('Adgangskode til erhverv.medicinpriser.dk (vises ikke): ')
        if not email or not password:
            raise SystemExit('Login must not be empty')
        values={'R_LIBS_USER':str(home/'monitorering-r-library'),'TZ':'Europe/Copenhagen',
                'MONITOR_RUNTIME':str(runtime),'MONITOR_DB':str(runtime/'monitorering.duckdb'),
                'MONITOR_ARCHIVE':str(runtime/'archive'),'MONITOR_LOG':str(runtime/'events.jsonl'),
                'MONITOR_CATALOG':str(catalog),'MONITOR_APP_RDS':str(app/'data/clean_data2.rds'),
                'MONITOR_APP_RESTART':str(app/'restart.txt'),
                'MEDPRIS_EMAIL':email,'MEDPRIS_PASSWORD':password}
        with tempfile.TemporaryDirectory(prefix='monitor-config-') as directory:
            temp=pathlib.Path(directory)/'config';temp.write_text(''.join(k+'='+envquote(v)+'\n' for k,v in values.items()))
            temp.chmod(0o600)
            sudo('install','-o','root','-g','root','-m','0600',str(temp),str(config))
    else:
        print('Existing root-only configuration retained; credentials were not replaced.')
    # Only data and restart marker become writable, never the Shiny source code.
    sudo('chown',account.pw_name+':shiny',str(app/'data'))
    sudo('chmod','2750',str(app/'data'))
    if not (app/'restart.txt').exists():
        sudo('touch',str(app/'restart.txt'))
    sudo('chown',account.pw_name+':shiny',str(app/'restart.txt'))
    sudo('chmod','0640',str(app/'restart.txt'))
    service=f'''[Unit]
Description=Monitorering validated daily tariff capture and app update
After=network-online.target
Wants=network-online.target
[Service]
Type=oneshot
User={account.pw_name}
Group={grp.getgrgid(account.pw_gid).gr_name}
WorkingDirectory={code}
EnvironmentFile=/etc/monitorering-priser.env
ExecStart={rscript} {code}/monitorering/scripts/update_tariff.R
UMask=0077
TimeoutStartSec=30min
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=read-only
ReadOnlyPaths={code}
ReadWritePaths={runtime} {app}/data {app}/restart.txt
'''
    timer='''[Unit]
Description=Daily Monitorering tariff update
[Timer]
OnCalendar=*-*-* 03:15:00 Europe/Copenhagen
Persistent=true
RandomizedDelaySec=120
[Install]
WantedBy=timers.target
'''
    with tempfile.TemporaryDirectory(prefix='monitor-units-') as directory:
        for name,text in [('monitorering-priser.service',service),('monitorering-priser.timer',timer)]:
            temp=pathlib.Path(directory)/name;temp.write_text(text)
            sudo('install','-o','root','-g','root','-m','0644',str(temp),'/etc/systemd/system/'+name)
    sudo('systemctl','daemon-reload')
    print('Installed. Timer is NOT enabled. Next: sudo systemctl start monitorering-priser.service')

if __name__=='__main__':
    main()

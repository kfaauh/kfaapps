import importlib.util
import pathlib
import unittest
root=pathlib.Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('activate',root/'admin/activate_tariff_job.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
class CronTests(unittest.TestCase):
 def test_successful_unloaded_oneshot(self):
  self.assertTrue(m.service_succeeded({'Result':'success','ExecMainStatus':'0','ExecMainStartTimestamp':''}))
  self.assertFalse(m.service_succeeded({'Result':'exit-code','ExecMainStatus':'1'}))
  self.assertFalse(m.service_succeeded({}))
 def test_only_legacy_price_collector_is_removed(self):
  other='30 3 * * * update_spc\n'
  line='0 3 * * * cd /srv/shiny-server/kfaapps/data-scripts && /usr/bin/Rscript fetch_data.R /srv/shiny-server/kfaapps/lmgrupper/data "Alle lægemidler.xlsx" >> /var/log/kfaapps_data_update.log 2>&1 && /usr/bin/Rscript fetch_data2.R /srv/shiny-server/kfaapps/lmgrupper2/data "Alle lægemidler.xlsx" >> /var/log/kfaapps_data_update.log 2>&1 && /usr/bin/Rscript data_management.R /srv/shiny-server/kfaapps/lmforbrug/data >> /var/log/kfaapps_data_update.log 2>&1 && chown shiny:shiny /srv/shiny-server/kfaapps/lmgrupper/data/clean_data.rds /srv/shiny-server/kfaapps/lmgrupper2/data/Clean_data2.rds\n'
  new,changed=m.detach_legacy(line+other)
  self.assertTrue(changed);self.assertNotIn('fetch_data2.R',new);self.assertNotIn('Clean_data2.rds',new)
  self.assertIn('fetch_data.R',new);self.assertIn('data_management.R',new);self.assertTrue(new.endswith(other))
  self.assertEqual(m.detach_legacy(new),(new,False))
 def test_unknown_layout_is_never_rewritten(self):
  with self.assertRaises(ValueError):m.detach_legacy('0 3 * * * Rscript fetch_data2.R some-other-path\n')
 def test_multiple_jobs_are_never_rewritten(self):
  with self.assertRaises(ValueError):m.detach_legacy('fetch_data2.R\nfetch_data2.R\n')
if __name__=='__main__':unittest.main()

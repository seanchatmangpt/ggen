%% knhks_rc.app — Application resource file
{application, knhks_rc,
 [{description, "KNHKS Reflexive Control (v1.0)"},
  {vsn, "1.0.0"},
  {registered, [knhks_rc_sup]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {knhks_rc_app, []}},
  {modules, []}]}.


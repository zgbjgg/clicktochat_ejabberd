%% -------------------------------------------------------------------
%% clicktochat.hrl : include for clicktochat
%% Copyright (c) 2012 All Rights Reserved.
%% Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
%% -------------------------------------------------------------------

% get proc for a module (implementing behavior 'gen_mod')
-define(get_proc(Host, Module), gen_mod:get_module_proc(Host, Module)).

% content type in http request
-define(content_type(Ctype), [{"Content-Type", Ctype}]).

% config file path for clicktochat 
% MY CONFIGURATION GOES HERE!!
-define(config_file, "/etc/ejabberd/clicktochat.conf").

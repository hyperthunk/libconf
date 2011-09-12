%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-record(check, {
    name                :: string(),
    require_values      :: [string()],
    require_templates   :: [string()],
    require_data        :: [term()]
}).

-record(template, {
    name        :: string(),
    data        :: string(),
    outfile     :: string(),
    checks      :: [#check{}]
}).

-record(library, {
    path  :: string(),
    lib   :: string(),
    arch  :: atom()
}).

-record(lib_conf, {
    dir         = unknown   :: string(),
    version     = unknown   :: string(),
    arch        = unknown   :: integer(),
    incl_path   = []        :: string(),
    ld_path                 :: undefined | {string(), string()}
}).

-type(arch() :: 'ia64' | 'amd64' | 'x86_64' | 'x86' | 'i386' | 'i686').

-record(os_conf, {
    os      = unknown   :: windows | darwin | unix,
    arch    = unknown   :: arch(),
    erlang  = unknown   :: [{atom(), term()}]
}).

-export_type([arch/0]).

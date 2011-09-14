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
    mandatory = false   :: boolean(),
    name                :: string(),
    type                :: 'include' | 'library',
    data                :: term(),
    capture             :: string() | binary(),
    output              :: string(),
    result              :: 'passed' | 'failed'
}).

-record(require, {
    path        = unknown   :: string(),
    arch        = unknown   :: integer(),
    find                    :: string(),
    include                 :: string(),
    incl_path               :: [string()],
    code_path               :: [string()]
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

-type(arch() :: 'x86_64' | 'x86').

-record(os_conf, {
    os          = unknown   :: 'unknown' | {atom(), 'unknown' | list(integer)},
    arch        = unknown   :: 'unknown' | arch(),
    wordsize    = unknown   :: 'unknown' | integer(),
    erlang      = unknown   :: 'unknown' | [{atom(), term()}]
}).

-export_type([arch/0]).

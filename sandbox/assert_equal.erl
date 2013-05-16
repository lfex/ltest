-module(assert_equal).
-export([assertEqual/2]).
% a call to
%
%   assertEqual(1, 2)
%
% will be replaced with this:
%
%       ((fun (__X) ->
%           case (2) of
%           __X -> ok;
%           __V -> .erlang:error({assertEqual_failed,
%                         [{module, ?MODULE},
%                          {line, ?LINE},
%                          {expression, (??2)},
%                          {expected, __X},
%                          {value, __V}]})
%           end
%         end)(1))).
%
%
%
% Here's the original:
%
% -define(assertEqual(Expect, Expr),
%     ((fun (__X) ->
%         case (Expr) of
%         __X -> ok;
%         __V -> .erlang:error({assertEqual_failed,
%                       [{module, ?MODULE},
%                        {line, ?LINE},
%                        {expression, (??Expr)},
%                        {expected, __X},
%                        {value, __V}]})
%         end
%       end)(Expect))).
assertEqual(Expect, Expr) ->
    ((fun (__X) ->
        case (Expr) of
        __X -> ok;
        __V -> .erlang:error({assertEqual_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expression, (Expr)},
                       {expected, __X},
                       {value, __V}]})
        end
      end)(Expect)).
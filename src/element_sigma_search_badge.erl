-module (element_sigma_search_badge).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
	reflect/0,
	render_element/1,
	event/1
]).

reflect() -> record_info(fields, sigma_search_badge).

render_element(#sigma_search_badge{id=Id,
                                   type=Type,
                                   text=Text}) ->
    Searches = string:tokens(wf:q(sigma_search_textbox), " "),
    Texts = string:tokens(Text,  " "),
    wf:set(sigma_search_textbox, Searches -- Texts ),
    Hiddens = string:tokens(wf:q(sigma_search_hidden), " "), 
    wf:set(sigma_search_hidden, string:join(lists:usort(Texts ++ Hiddens), " ")),
    #panel{id=Id,
           class=["sigma_search_badge", "badge"],
           body=[
                 #span{class=sigma_search_badge_type, text=Type}, 
                ":",
                #span{class=sigma_search_badge_text, text= Text},
                #span{ class="", text="  x", actions=#event{
                                                        type=click,
                                                        postback={remove, Id, Text},
                                                        delegate=?MODULE
                                                       }}
                 
                ]}.

event({remove, Id, Text}) -> % {{{1
    Data = wf:q(sigma_search_hidden),
    wf:console_log({Data}),
    Terms = string:tokens(Data, " "),
    DTerms = string:tokens(Text, " "),
    wf:set(sigma_search_hidden, string:join(Terms -- DTerms, " ")),
    wf:remove(Id);
event(E) -> % {{{1
    error_logger:error("Wrong event ~p in ~p~n", [E, ?MODULE]).
    

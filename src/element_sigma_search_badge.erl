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
    Search = wf:q(sigma_search_textbox),
    Searches = string:tokens(Search, " "),
    Texts = string:tokens(Text,  " "),
    Hiddens = wf:state_default(sigma_search_hidden, ""),
    NHiddens = lists:usort(Hiddens ++ Texts),
    NSearches = Searches -- NHiddens,

    wf:state(sigma_search_hidden, NHiddens),
    wf:set(sigma_search_textbox, string:join(NSearches, " ")),

    #panel{id=Id,
           class=["sigma_search_badge", "badge"],
           style="border-radius:3px;border-left:2px #fff solid;",
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
    Terms = wf:state(sigma_search_hidden),
    DTerms = string:tokens(Text, " "),
    wf:state(sigma_search_hidden, Terms -- DTerms),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
    wf:remove(Id);
event(E) -> % {{{1
    error_logger:error("Wrong event ~p in ~p~n", [E, ?MODULE]).
    

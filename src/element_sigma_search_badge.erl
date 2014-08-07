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
    %wf:set(".sigma_search_textbox", ""),
    #panel{id=Id,
           class=["sigma_search_badge", "badge"],
           body=[
                 #span{class=sigma_search_badge_type, text=Type}, 
                ":",
                #span{class=sigma_search_badge_text, text= Text},
                #span{ class="", text="  x", actions=#event{
                                                        type=click,
                                                        postback={remove, Id},
                                                        delegate=?MODULE
                                                       }}
                 
                ]}.

event({remove, Id}) -> % {{{1
    wf:remove(Id);
event(E) -> % {{{1
    error_logger:error("Wrong event ~p in ~p~n", [E, ?MODULE]).
    

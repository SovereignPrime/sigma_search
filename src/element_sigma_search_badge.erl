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
                                   dropdown=Dropdown,
                                   text=Text}=B) ->
    Search = wf:q(sigma_search_textbox),
    Searches = string:tokens(Search, " "),
    Texts = string:tokens(Text,  " "),
    Hiddens = wf:state_default(sigma_search_hidden, ""),
    NHiddens = lists:usort(Hiddens ++ [{Type, Text}]),
    NSearches = Searches -- Texts,
    wf:info("~p~n", [ NHiddens ]),

    wf:state(sigma_search_hidden, NHiddens),
    wf:set(sigma_search_textbox, string:join(NSearches, " ")),

    #panel{id=Id,
           class=["sigma_search_badge", "badge"],
           style="border-radius:3px;border-left:2px #fff solid;",
           body=[case Dropdown of
                     [] ->
                         #span{class=sigma_search_badge_type,
                               body=Type}; 
                     _ ->
                         #span{class=sigma_search_badge_type,
                               body=[
                                     #panel{class="btn-group",
                                            body=[
                                                  #link{class="btn dropdown-toggle btn-link btn-inverse",
                                                        style="color: #fff;line-height:14px;padding:0;font-size:12px;",
                                                        body=Type,
                                                        data_fields=[{toggle, "dropdown"}],
                                                        url="#",
                                                        new=false},
                                                  #list{numbered=false,
                                                        class="dropdown-menu",
                                                        body=lists:map(fun(D) ->
                                                                               #listitem{class="",
                                                                                         body=[
                                                                                               #link{text=D,
                                                                                                     postback={dropdown, B, D},
                                                                                                     delegate=?MODULE}
                                                                                              ]}
                                                                       end, Dropdown)
                                                       }
                                                 ]},
" <i class='icon icon-caret-down icon-inverse'></i>"
                                    ]}
                 end,
                 #span{body=[ " | "]},
                 #span{style="vertical-align:middle;", class=sigma_search_badge_text, text= Text},
                 #span{class="",
                       text="  x",
                       actions=#event{
                                  type=click,
                                  postback={remove, B},
                                  delegate=?MODULE
                                 }}

                ]}.

event({dropdown, #sigma_search_badge{id=Id, type=OType, text=Text}=Badge, Type}) -> % {{{1
    Badges = wf:state('sigma_search_hidden'),
    wf:state(sigma_search_hidden, Badges -- [{OType, Text}]),
    wf:replace(Id, Badge#sigma_search_badge{type=Type});
event({remove, #sigma_search_badge{id=Id, type=Type, text=Text}}) -> % {{{1
    Terms = wf:state(sigma_search_hidden),
    wf:state(sigma_search_hidden, Terms -- [ {Type, Text} ]),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
    wf:remove(Id);
event(E) -> % {{{1
    error_logger:error("Wrong event ~p in ~p~n", [E, ?MODULE]).
    

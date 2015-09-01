-module (element_sigma_search_badge).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
	reflect/0,
	render_element/1,
	event/1
]).

reflect() -> record_info(fields, sigma_search_badge).

render_element(#sigma_search_badge{id=Id,  % {{{1
                                   textboxid=Textboxid,
                                   type="Term",
                                   text=Text}=B) ->
    wf:info("Term ~p", [Text]),
    wf:set(Textboxid, Text),
    [];
render_element(#sigma_search_badge{id=Id,  % {{{1
                                   textboxid=Textboxid,
                                   type=Type,
                                   dropdown=Dropdown,
                                   text=Text}=B) ->
    Hiddens = wf:session_default(Textboxid, ""),
    NHiddens = lists:usort(Hiddens ++ [{Type, Text}]),

    wf:session(Textboxid, NHiddens),

    #panel{id=Id,
           class=["sigma_search_badge", "badge"],
           style="border-radius:3px;border-left:2px #fff solid; float:left;",
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
                                                        body=[
                                                              Type,
                                                              " <i class='icon icon-caret-down icon-inverse'></i>"
                                                             ],
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
                                                                       end,
                                                                       Dropdown)
                                                       }
                                                 ]}
                                    ]}
                 end,
                 #span{body=[ " | "]},
                 #span{style="vertical-align:middle;",
                       class=sigma_search_badge_text,
                       text= Text},
                 #span{class="",
                       text="  x",
                       actions=#event{
                                  type=click,
                                  postback={remove, B},
                                  delegate=?MODULE
                                 }}

                ]}.

event({dropdown, % {{{1
       #sigma_search_badge{id=Id,
                           textboxid=Textboxid,
                           type=OType,
                           text=Text}=Badge,
       Type}) ->
    Badges = wf:session(Textboxid),
    wf:session(Textboxid, Badges -- [{OType, Text}] ++ [{Type, Text}]),
    wf:replace(Id, Badge#sigma_search_badge{type=Type}),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"});

event({remove, #sigma_search_badge{id=Id, % {{{1
                                   textboxid=Textboxid,
                                   type=Type,
                                   text=Text}}) ->
    Terms = wf:session(Textboxid),
    wf:session(Textboxid, Terms -- [ {Type, Text} ]),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
    wf:remove(Id);
event(E) -> % {{{1
    error_logger:error("Wrong event ~p in ~p~n", [E, ?MODULE]).
    

-module (element_sigma_search).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
	reflect/0,
	render_element/1,
	event/1
]).

-record(postback,
        {
         id,
         results_summary_text,
         results_summary_class,
         textboxid, 
         clearid,
         resultsid,
         badgesid,
         hiddenid,
         show_results=true,
         search_button_id,
         tag,
         delegate,
         x_button_text,
         x_button_class
        }).

reflect() -> record_info(fields, sigma_search).


render_element(Rec = #sigma_search{
                        id=Id,
                        placeholder=Placeholder,
                        textbox_class=TextboxClass,
                        results_class=ResultsClass,
                        style=Style,
                        search_button_class=SearchButtonClass,
                        search_button_text=SearchButtonText,
                        clear_button_class=ClearClass,
                        clear_button_text=ClearText,
                        tag=Tag,
                        badges=Badges,
                        class=WrapperClass,
                        results_summary_text=ResultsSummaryText,
                        results_summary_class=ResultsSummaryClass,
                        x_button_class=XButtonClass,
                        x_button_text=XButtonText
                       }) ->
	MyId = wf:coalesce([Id, wf:temp_id()]),
	Textboxid = wf:temp_id(),
	Resultsid = wf:temp_id(),
    BadgesId = wf:temp_id(),
	SearchButtonid = wf:temp_id(),
	Clearid = wf:temp_id(),
	Delegate = wf:coalesce([Rec#sigma_search.delegate, wf:page_module()]),
	
	Postback = #postback{
                  id=MyId,
                  delegate=Delegate,
                  tag=Tag,
                  textboxid=Textboxid,
                  resultsid=Resultsid,
                  badgesid=BadgesId,
                  clearid=Clearid,
                  search_button_id=SearchButtonid,
                  results_summary_text=ResultsSummaryText,
                  results_summary_class=ResultsSummaryClass,
                  x_button_text=XButtonText,
                  x_button_class=XButtonClass
                 },

    wf:wire(#script{script="$(window).resize(function() {" ++
                    length_adjust(MyId,
                                  Textboxid, 
                                  Resultsid,
                                  BadgesId,
                                  SearchButtonid,
                                  Clearid
                                 ) ++
    "})"}),
    wf:wire(#event{type=timer,
                   delay=200,
                   actions=#script{script=length_adjust(MyId,
                                                        Textboxid, 
                                                        Resultsid,
                                                        BadgesId,
                                                        SearchButtonid,
                                                        Clearid
                                                       )}}),
    wf:wire(#event{type=timer,
                   delay=200,
                   postback=Postback#postback{show_results=false},
                   delegate=?MODULE}),
    [
     #panel{
        id=MyId,
        class=["sigma_search", WrapperClass],
        style=Style,
        body=[
              #panel{
                 id=BadgesId,
                 style="float:left;background:transparent;",
                 class=["wfid_sigma_search_badges", "sigma_search_badges", "add-on"],
                 body=lists:map(fun(B) ->
                                        B#sigma_search_badge{
                                          textboxid=Textboxid
                                         }
                                end,
                                Badges)
                },
              #textbox{
                 class=[sigma_search_textbox,
                        wfid_sigma_search_textbox,
                        TextboxClass],

                 postback=Postback,
                 style="box-shadow: none;",
                 delegate=?MODULE,
                 id=Textboxid,
                 placeholder=Placeholder,
                 actions=[
                          #event{type=keydown,
                                 postback=Postback,
                                 delegate=?MODULE}
                         ]
                },
              #button{
                 id=SearchButtonid,
                 style="float:right;",
                 class=[sigma_search_button, SearchButtonClass],
                 body=SearchButtonText,
                 postback={filter, Postback#postback{show_results=false}},
                 delegate=?MODULE
                },
              #button{
                 id=Clearid,
                 body=ClearText,
                 class=[sigma_search_clear, ClearClass],
                 style="display:none",
                 click=[
                        #set{target=Textboxid, value=""},
                        #script{script="$(obj('" ++ BadgesId ++ "')).empty()"},
                        #fade{target=Resultsid},
                        #fade{target=Clearid}
                       ],
                 postback={search_clear, Postback, Delegate},
                 delegate=?MODULE
                }
             ]},
     #panel{
        id=Resultsid,
        class=[sigma_search_results, wfid_sigma_search_results, ResultsClass],
        style="display:none"
       }
    ].

event(#postback{
         id=MyId,
         delegate=Delegate,
         tag=Tag,
         textboxid=Textboxid,
         resultsid=Resultsid,
         badgesid=BadgesId,
         hiddenid=HiddenId,
         clearid=Clearid,
         show_results=ShowResult,
         search_button_id=SearchButtonid,
         results_summary_text=ResultsSummaryText,
         results_summary_class=ResultsSummaryClass,
         x_button_class=XButtonClass,
         x_button_text=XButtonText
	}) ->
    case {string:strip(wf:q(Textboxid)), wf:session_default(Textboxid, "")} of
        {"", ""} -> 
			wf:wire(Resultsid,#fade{}),
            wf:wire(Clearid, #fade{});
        {Search, Hidden} ->
            {TBadges, Body} = Delegate:sigma_search_event(Tag, Hidden ++ [{"Term", Search}]),

            Badges = lists:map(fun(B) ->
                                       B#sigma_search_badge{
                                          textboxid=Textboxid
                                         }
                               end,
                               TBadges),
			ResultsBody = [
				#button{
					text=XButtonText,
					class=[sigma_search_x_button, XButtonClass],
					click="objs('" ++ Clearid ++ "').click()"
				},
				Body
			],
			wf:update(Resultsid, ResultsBody),
            wf:session(Textboxid, ""),
			wf:update(BadgesId, Badges),
			wf:wire(Clearid, #appear{}),
            wf:wire(#script{script=length_adjust(MyId,
                                                 Textboxid, 
                                                 Resultsid,
                                                 BadgesId,
                                                 SearchButtonid,
                                                 Clearid
                                                )}),
            case ShowResult andalso Body /= [] of
                true ->
                    wf:wire(Resultsid, #slide_down{});
                _ ->
                    wf:wire(Resultsid,#fade{})
            end
	end;
event({filter, #postback{
                  id=MyId,
                  delegate=Delegate,
                  tag=Tag,
                  textboxid=Textboxid,
                  resultsid=Resultsid,
                  badgesid=BadgesId,
                  hiddenid=HiddenId,
                  clearid=Clearid,
                  results_summary_text=ResultsSummaryText,
                  results_summary_class=ResultsSummaryClass,
                  x_button_class=XButtonClass,
                  x_button_text=XButtonText
                 }}) ->
			wf:wire(Resultsid,#fade{}),
            Term = wf:q(Textboxid),
            Hidden = wf:session_default(Textboxid, ""),
            Delegate:sigma_search_filter_event(Tag, Hidden ++ [{"Term", Term}]);
event({search_clear, #postback{
                        id=MyId,
                        textboxid=Textboxid,
                        resultsid=Resultsid,
                        badgesid=BadgesId,
                        hiddenid=HiddenId,
                        search_button_id=SearchButtonid,
                        clearid=Clearid
                       }, Delegate}) ->
    wf:wire(#script{script=length_adjust(MyId,
                                         Textboxid, 
                                         Resultsid,
                                         BadgesId,
                                         SearchButtonid,
                                         Clearid
                                         )}),
    Delegate:sigma_search_filter_clear(),
    wf:session(Textboxid, "").

length_adjust(Id, Textboxid, Resultsid, BadgesId, SearchButtonid, Clearid) ->
    "$(obj('" ++ Textboxid 
       ++ "')).width($(obj('" 
       ++ Id 
       ++ "')).innerWidth() - $(obj('"
       ++ BadgesId 
       ++ "')).outerWidth() - $(obj('" 
       ++ SearchButtonid 
       ++ "')).outerWidth() - $(obj('" 
       ++ Clearid 
       ++ "')).outerWidth() - $('.sigma_search_x_button').outerWidth() - 10);
        $(obj('" ++ Resultsid ++ "')).width($(obj('" 
       ++ Id 
       ++ "')).innerWidth() - 7);
       ".

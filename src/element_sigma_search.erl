-module (element_sigma_search).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
	reflect/0,
	render_element/1,
	event/1
]).

-record(postback, {
		results_summary_text,
		results_summary_class,
		textboxid, 
		clearid,
		resultsid,
        badgesid,
        hiddenid,
		tag,
		delegate,
		x_button_text,
		x_button_class
	}).

reflect() -> record_info(fields, sigma_search).


render_element(Rec = #sigma_search{
		placeholder=Placeholder,
		textbox_class=TextboxClass,
		results_class=ResultsClass,
		search_button_class=SearchButtonClass,
		search_button_text=SearchButtonText,
		clear_button_class=ClearClass,
		clear_button_text=ClearText,
		tag=Tag,
		class=WrapperClass,
		results_summary_text=ResultsSummaryText,
		results_summary_class=ResultsSummaryClass,
		x_button_class=XButtonClass,
		x_button_text=XButtonText
		}) ->
	Textboxid = wf:temp_id(),
	Resultsid = wf:temp_id(),
    BadgesId = wf:temp_id(),
	SearchButtonid = wf:temp_id(),
	Clearid = wf:temp_id(),
	Delegate = wf:coalesce([Rec#sigma_search.delegate, wf:page_module()]),
	
	Postback = #postback{
		delegate=Delegate,
		tag=Tag,
		textboxid=Textboxid,
		resultsid=Resultsid,
        badgesid=BadgesId,
		clearid=Clearid,
		results_summary_text=ResultsSummaryText,
		results_summary_class=ResultsSummaryClass,
		x_button_text=XButtonText,
		x_button_class=XButtonClass
	},

    wf:wire(#script{script="$(window).resize(function() {" ++
                    length_adjust(".wfid_" ++ BadgesId,
                                  ".wfid_" ++ SearchButtonid,
                                  ".wfid_" ++ Clearid) ++
    "})"}),
    wf:wire(#event{type=timer,
                   delay=200,
                   actions=#script{script=length_adjust(".wfid_" ++ BadgesId, ".wfid_" ++ SearchButtonid, ".wfid_" ++ Clearid)}}),
    wf:session(sigma_search_result_show, false),
    wf:wire(#event{type=timer,
                   delay=200,
                   postback=Postback,
                   delegate=?MODULE}),
    [
		#panel{class=["sigma_search", WrapperClass],
               body=[
                     #panel{
                        id=BadgesId,
                        style="float:left;background:transparent;",
                        class=["wfid_sigma_search_badges", "sigma_search_badges", "add-on"],
                        body=[]
                       },
            #textbox{
                class=[sigma_search_textbox, wfid_sigma_search_textbox, TextboxClass],
                postback=Postback,
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
				postback={filter, Postback},
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
	}) ->
    case {string:strip(wf:q(Textboxid)), wf:session_default(Textboxid, "")} of
        {"", ""} -> 
			wf:wire(Resultsid,#fade{}),
            Delegate:sigma_search_filter_clear(),
            wf:wire(Clearid, #fade{});
        {Search, Hidden} ->
            {TBadges, Body} = Delegate:sigma_search_event(Tag, Hidden ++ [{"Term", Search}]),

            Badges = lists:map(fun({Type, Text, Variants}) ->
                                       #sigma_search_badge{
                                          textboxid=Textboxid,
                                          type=Type,
                                          text=Text,
                                          dropdown=Variants -- [Type]
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
            wf:wire(#script{script=length_adjust(".wfid_" ++ BadgesId,
                                                 ".sigma_search_button",
                                                 ".wfid_" ++ Clearid)}),
            case  wf:session_default(sigma_search_result_show, true) of
                true ->
                    wf:session(sigma_search_result_show, true),
                    wf:wire(Resultsid, #slide_down{});
                _ ->
                    wf:session(sigma_search_result_show, true)
            end
	end;
event({filter, #postback{
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
event({search_clear, #postback{textboxid=Textboxid}, Delegate}) ->
    wf:wire(#script{script=length_adjust(".sigma_search_badges",
                                         ".sigma_search_button",
                                         ".sigma_search_clear")}),
    Delegate:sigma_search_filter_clear(),
    wf:session(Textboxid, "").

length_adjust(BadgesId, SearchButtonid, Clearid) ->
    "$('.sigma_search_textbox').width($('.sigma_search').innerWidth() - $('"
       ++ BadgesId 
       ++ "').outerWidth() - $('" 
       ++ SearchButtonid 
       ++ "').outerWidth() - $('" 
       ++ Clearid 
       ++ "').outerWidth() - $('.sigma_search_x_button').outerWidth() - 10);
        $('.sigma_search_results').width($('.sigma_search').innerWidth() - 7);
       ".

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

	[
		#panel{class=WrapperClass, body=[
            #panel{
               id=BadgesId,
               style="float:left;background:transparent;",
               class=["sigma_search_badges", "add-on"],
               body=[]
              },
            #textbox{
                class=[sigma_search_textbox, wfid_sigma_search_textbox, TextboxClass],
                postback=Postback,
                delegate=?MODULE,
                id=Textboxid,
                placeholder=Placeholder,
                actions=[
                    #event{type=keydown,postback=Postback,delegate=?MODULE}
                ]
            },
			#button{
				id=SearchButtonid,
                style="float:right;",
				class=[sigma_search_button, SearchButtonClass],
				body=SearchButtonText,
				postback=Postback,
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
                postback=search_clear,
                delegate=?MODULE
			}
		]},
		#panel{
			id=Resultsid,
			class=[sigma_search_results, ResultsClass],
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
    case {wf:q(Textboxid), wf:state_default(sigma_search_hidden, "")} of
        {"", ""} -> 
			wf:wire(Resultsid,#fade{}),
			wf:wire(Clearid, #fade{});
        {Search, Hidden} ->
            {Badges, Body} = Delegate:sigma_search_event(Tag, Hidden ++ [{"Term", Search}]),

			ResultsBody = [
				#button{
					text=XButtonText,
					class=[sigma_search_x_button, XButtonClass],
					click="objs('" ++ Clearid ++ "').click()"
				},
				Body
			],
			wf:update(Resultsid, ResultsBody),
            wf:state(sigma_search_hidden, ""),
			wf:update(BadgesId, Badges),
			wf:wire(Clearid, #appear{}),
			wf:wire(Resultsid, #slide_down{})
	end;
event(search_clear) ->
    wf:state(sigma_search_hidden, "").

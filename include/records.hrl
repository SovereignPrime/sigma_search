-record(sigma_search, {?ELEMENT_BASE(element_sigma_search),
						delegate,
						placeholder="",
						tag,
						textbox_class="",
						search_button_class="",
						search_button_text="Search",
						clear_button_class="",
						clear_button_text="Clear",
						results_class="",
						results_summary_text="~p search results for \"~s\"",
						results_summary_class="",
						x_button_text="X",
						x_button_class=""
		}).
-record(sigma_search_badge, {?ELEMENT_BASE(element_sigma_search_badge),
						delegate,
                        type="",
                        text=""
		}).

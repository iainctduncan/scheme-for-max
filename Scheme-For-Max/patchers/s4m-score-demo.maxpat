{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 8,
			"minor" : 6,
			"revision" : 5,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"classnamespace" : "box",
		"rect" : [ 53.0, 87.0, 990.0, 842.0 ],
		"bglocked" : 0,
		"openinpresentation" : 0,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "",
		"assistshowspatchername" : 0,
		"boxes" : [ 			{
				"box" : 				{
					"fontsize" : 15.0,
					"id" : "obj-56",
					"linecount" : 2,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 200.0, 480.0, 559.0, 40.0 ],
					"presentation_linecount" : 2,
					"text" : "iain C.T. Duncan - 2025\nhttp://github.com/iainctduncan/scheme-for-max"
				}

			}
, 			{
				"box" : 				{
					"fontsize" : 15.0,
					"id" : "obj-55",
					"linecount" : 3,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 208.0, 61.5, 560.0, 57.0 ],
					"presentation_linecount" : 16,
					"text" : "Uses the Scheme file s4m-score-demo.scm, depends on s4m-score.scm\nUse the plug message to load VST instrument for playback\nClick the (play-score-1) etc. buttons to start playback."
				}

			}
, 			{
				"box" : 				{
					"fontface" : 1,
					"fontsize" : 24.0,
					"id" : "obj-54",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 208.0, 11.0, 303.0, 33.0 ],
					"text" : "S4M-Score Demo Patch"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-52",
					"linecount" : 2,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 50.0, 166.0, 106.0, 33.0 ],
					"text" : "click below to test VST output"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-50",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 495.5, 136.0, 83.0, 22.0 ],
					"text" : "(play-score-3)"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-47",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 398.5, 136.0, 83.0, 22.0 ],
					"text" : "(play-score-2)"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-46",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 560.0, 292.5, 52.0, 20.0 ],
					"text" : "'locate"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-45",
					"linecount" : 3,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 743.0, 213.0, 150.0, 47.0 ],
					"text" : "Clicking metro to demonstrate sync with score sequencer"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-43",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 23.0, 68.0, 77.0, 20.0 ],
					"text" : "'s4m-reset"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"format" : 6,
					"id" : "obj-49",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 422.0, 352.0, 41.600000619888306, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-48",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 305.0, 230.0, 71.200001060962677, 20.0 ],
					"text" : "'tempo-line"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-42",
					"maxclass" : "newobj",
					"numinlets" : 3,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"patching_rect" : [ 259.0, 229.0, 40.0, 22.0 ],
					"text" : "line",
					"varname" : "tempo-line"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-38",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 259.0, 260.0, 59.0, 22.0 ],
					"text" : "tempo $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-40",
					"maxclass" : "toggle",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "int" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 743.0, 270.0, 23.099999904632568, 23.099999904632568 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-39",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 810.0, 301.0, 52.800000786781311, 20.0 ],
					"text" : "'metro"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-35",
					"lastchannelcount" : 0,
					"maxclass" : "live.gain~",
					"numinlets" : 2,
					"numoutlets" : 5,
					"orientation" : 1,
					"outlettype" : [ "signal", "signal", "", "float", "list" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 742.0, 394.0, 73.0, 47.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "live.gain~[1]",
							"parameter_mmax" : 6.0,
							"parameter_mmin" : -70.0,
							"parameter_modmode" : 3,
							"parameter_shortname" : "live.gain~",
							"parameter_type" : 0,
							"parameter_unitstyle" : 4
						}

					}
,
					"varname" : "live.gain~[1]"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-34",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "signal" ],
					"patching_rect" : [ 742.0, 352.0, 39.0, 22.0 ],
					"text" : "click~"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-32",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 516.0, 378.0, 72.0, 22.0 ],
					"text" : "prepend set"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-9",
					"maxclass" : "button",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 840.0, 342.0, 42.0, 42.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-27",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 742.0, 301.0, 60.0, 23.0 ],
					"text" : "metro 4n",
					"varname" : "metro"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-18",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 257.5, 136.0, 35.0, 22.0 ],
					"text" : "reset",
					"varname" : "s4m-reset[1]"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-15",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 304.5, 136.0, 83.0, 22.0 ],
					"text" : "(play-score-1)"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-3",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 257.5, 177.0, 61.0, 22.0 ],
					"text" : "s #0_s4m"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-1",
					"maxclass" : "newobj",
					"numinlets" : 0,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 63.0, 97.0, 59.0, 22.0 ],
					"text" : "r #0_s4m"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-29",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 516.0, 291.5, 42.0, 22.0 ],
					"varname" : "locate"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-28",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 23.0, 97.0, 35.0, 22.0 ],
					"text" : "reset",
					"varname" : "s4m-reset"
				}

			}
, 			{
				"box" : 				{
					"attr" : "tempo",
					"fontface" : 0,
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-19",
					"maxclass" : "attrui",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 338.0, 260.0, 152.0, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"format" : 6,
					"id" : "obj-33",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 604.0, 378.0, 79.0, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-31",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 2,
					"outlettype" : [ "bang", "" ],
					"patching_rect" : [ 215.0, 294.0, 39.0, 23.0 ],
					"text" : "sel 0"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-30",
					"maxclass" : "toggle",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "int" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 516.0, 353.0, 20.0, 20.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-26",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 473.0, 352.0, 30.249997019767761, 23.0 ],
					"text" : "4 4"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"format" : 6,
					"id" : "obj-24",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 327.0, 352.0, 48.800000727176666, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-20",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 295.0, 352.0, 28.800000429153442, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-21",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 259.0, 352.0, 32.000000476837158, 23.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-13",
					"maxclass" : "toggle",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "int" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 215.0, 259.0, 25.0, 25.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-22",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 9,
					"outlettype" : [ "int", "int", "float", "float", "float", "", "int", "float", "" ],
					"patching_rect" : [ 259.0, 324.0, 357.0, 23.0 ],
					"text" : "transport",
					"varname" : "transport"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-23",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "bang" ],
					"patching_rect" : [ 280.0, 294.0, 210.0, 23.0 ],
					"text" : "metro @interval 1 ticks @active 1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-17",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 6.0, 41.0, 97.0, 22.0 ],
					"text" : "udpreceive 7000"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-14",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 50.0, 201.0, 62.0, 22.0 ],
					"text" : "60 90 120"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-12",
					"maxclass" : "ezdac~",
					"numinlets" : 2,
					"numoutlets" : 0,
					"patching_rect" : [ 34.0, 464.0, 45.0, 45.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-11",
					"lastchannelcount" : 0,
					"maxclass" : "live.gain~",
					"numinlets" : 2,
					"numoutlets" : 5,
					"orientation" : 1,
					"outlettype" : [ "signal", "signal", "", "float", "list" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 34.0, 404.0, 94.0, 47.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_longname" : "live.gain~",
							"parameter_mmax" : 6.0,
							"parameter_mmin" : -70.0,
							"parameter_modmode" : 3,
							"parameter_shortname" : "live.gain~",
							"parameter_type" : 0,
							"parameter_unitstyle" : 4
						}

					}
,
					"varname" : "live.gain~"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-10",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 0.0, 323.0, 31.0, 22.0 ],
					"text" : "plug"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-8",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 3,
					"outlettype" : [ "int", "int", "int" ],
					"patching_rect" : [ 34.0, 231.0, 77.0, 22.0 ],
					"text" : "unpack 0 0 0"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-4",
					"maxclass" : "newobj",
					"numinlets" : 7,
					"numoutlets" : 2,
					"outlettype" : [ "int", "" ],
					"patching_rect" : [ 34.0, 322.0, 108.0, 23.0 ],
					"text" : "midiformat"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-6",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 34.0, 292.0, 32.5, 23.0 ],
					"text" : "join"
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 13.0,
					"id" : "obj-7",
					"maxclass" : "newobj",
					"numinlets" : 3,
					"numoutlets" : 2,
					"outlettype" : [ "float", "float" ],
					"patching_rect" : [ 34.0, 262.0, 66.0, 23.0 ],
					"text" : "makenote"
				}

			}
, 			{
				"box" : 				{
					"autosave" : 1,
					"bgmode" : 0,
					"border" : 0,
					"clickthrough" : 0,
					"id" : "obj-5",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 8,
					"offset" : [ 0.0, 0.0 ],
					"outlettype" : [ "signal", "signal", "", "list", "int", "", "", "" ],
					"patching_rect" : [ 34.0, 362.0, 92.5, 22.0 ],
					"save" : [ "#N", "vst~", "loaduniqueid", 0, ";" ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_invisible" : 1,
							"parameter_longname" : "vst~",
							"parameter_modmode" : 0,
							"parameter_shortname" : "vst~",
							"parameter_type" : 3
						}

					}
,
					"saved_object_attributes" : 					{
						"parameter_enable" : 1,
						"parameter_mappable" : 0
					}
,
					"snapshot" : 					{
						"filetype" : "C74Snapshot",
						"version" : 2,
						"minorversion" : 0,
						"name" : "snapshotlist",
						"origin" : "vst~",
						"type" : "list",
						"subtype" : "Undefined",
						"embed" : 1,
						"snapshot" : 						{
							"pluginname" : "Zebra2.vstinfo",
							"plugindisplayname" : "Zebra 2.9.3",
							"pluginsavedname" : "",
							"pluginsaveduniqueid" : 0,
							"version" : 1,
							"isbank" : 0,
							"isbase64" : 1,
							"blob" : "26765.CMlaKA....fQPMDZ....ALUSDIC...P.....AjlaoQWZgwVZ5UF..........................fVTiDTS8nUYhIWXxnvHVUlby0iLvTCLvnvHE4FYoEla8vVZzQGakovHt0VOyPiBiz1b831atUlBiz1b8zzajcEZrovHsMWOPkFcig1UJLRay0yPzIGaAovHsMWOCQmbrIjBiz1b8vjYucTLJLRay0CSl81QxnvHsMWOGEFckovHsMWOKUVdF8FaJLRay0yRkkmQuwlLJLRay0iUkw1aikFc4ovHsMWOAQ0a0MFZJLRay0SPxAWSuQlBiz1b8Djbv0DYxnvHsMWOE4lcwnvHsMWOE4lcxnvHsMWOE4lcynvHsMWOE4lcznvHsMWOMMUQGEiBiz1b8zzTEcjLJLRay0SSSUzQynvHsMWOMMUQGQiBiz1b8vjYuEiBiz1b8vjYuIiBiz1b8vjYuMiBiz1b8vjYuQiBiz1b8zTSgAWLJLRay0SSMEFbxnvHsMWOM0TXvMiBiz1b8zTSgAGMJLRay0SSMkFdwnvHsMWOM0TZ3IiBiz1b8zTSog2LJLRay0SSMkFdznvHtYWO0nvHsYWOGEFckovHsYWOE4lcwnvHsYWOE4lcxnvHsYWOE4lcynvHsYWOE4lcznvHi0VOsEVZtovPi8Db8fCMt.CLJLBSF8zQ8DiBivjQOcjL8DiBiLVa8.0PuIWYJf0WwzCLt.CLJj0WwzCLt.CLJf0WxzCLt.CLJj0WxzCLt.CLJf0WyzCLt.CLJj0WyzCLt.CLJf0WzzCLt.CLJj0WzzCLt.CLJzDUwDSOOM0PwnCU04VYJzDSwDSOv3hLvnPSRESL8zBLtHCLJzDUwHSOOM0PwnCQzUmaJzDSwHSOsjiKv.iBMIULxzSKwDiKv.iBMQULyzySSMjL5PDc04lBMwTLyzyMt.CLJzjTwLSO33BLvnPSTECM831atUlNgM2boclakQlBMwTLzzSMv3BLvnPSRECM8zRMv3BLvnPSTESM831atUlNgM2boclakQlBMwTL0zCM33BLvnPSRESM8zBM33BLvnPSTEiM831atUlNgM2boclakQlBMwTL1zSMv3BLvnPSREiM8zRMv3BLvnPSTEyM831atUlNgM2boclakQlBMwTL2zSMv3BLvnPSREyM8zRMv3BLvnPSTECN831atUlNgM2boclakQlBMwTL3zSMv3BLvnPSRECN8zRMv3BLvnPSTISL8TjSVMiNSU2bJzDSxDSO1HiKv.iBMIkLwzSKyTiKv.iBMQkLxziau4VY5D1byk1YtUFYJzDSxHSO0.iKv.iBMIkLxzSK0.iKv.iBMQkLyziau4VY5D1byk1YtUFYJzDSxLSO0.iKv.iBMIkLyzSK0.iKv.iBMQkLzziau4VY5D1byk1YtUFYJzDSxPSO0.iKv.iBMIkLzzSK0.iKv.iBMQkL0ziau4VY5D1byk1YtUFYJzDSxTSO0.iKv.iBMIkL0zSK0.iKv.iBMQkL1ziau4VY5D1byk1YtUFYJzDSxXSO0.iKv.iBMIkL1zSK0.iKv.iBMQkL2ziau4VY5D1byk1YtUFYJzDSxbSO0.iKv.iBMIkL2zSK0.iKv.iBMQkL3ziau4VY5D1byk1YtUFYJzDSxfSO0.iKv.iBMIkL3zSK0.iKv.iBMQ0LwziUCYTL5XTSwnPSLMSL8PCNt.CLJzjTyDSOsbiLt.CLJzDUyHSOt8lakoSXyMWZm4VYjoPSLMiL8TCLt.CLJzjTyHSOsTCLt.CLJzDUyLSOt8lakoSXyMWZm4VYjoPSLMyL8TCLt.CLJzjTyLSOsTCLt.CLJzDUyPSOt8lakoSXyMWZm4VYjoPSLMCM8TCLt.CLJzjTyPSOsTCLt.CLJzDUyTSOt8lakoSXyMWZm4VYjoPSLMSM8PCNt.CLJzjTyTSOsPCNt.CLJzDUyXSOt8lakoSXyMWZm4VYjoPSLMiM8TCLt.CLJzjTyXSOsTCLt.CLJzDUybSOt8lakoSXyMWZm4VYjoPSLMyM8PCNt.CLJzjTybSOsPCNt.CLJzDUyfSOt8lakoSXyMWZm4VYjoPSLMCN8TCLt.CLJzjTyfSOsTCLt.CLJzDUzDSOVMjQwniTkMmBMwDMwzyL43BLvnPSRQSL8HCNt.CLJzDUzHSOVMTPwniUuwlLJzDSzHSOv3BLvnPSRQiL8LSMt.CLJzDUzLSOVMTPwniUuwVLJzDSzLSOv3BLvnPSRQyL8zRL23BLvnPSTQCM831atUlNgM2boclakQlBMwDMzzCM33BLvnPSRQCM8zBM33BLvnPSTQSM831atUlNgM2boclakQlBMwDM0zSMv3BLvnPSRQSM8zRMv3BLvnPSTQiM831atUlNgM2boclakQlBMwDM1zCM33BLvnPSRQiM8zBM33BLvnPSTQyM831atUlNgM2boclakQlBMwDM2zSMv3BLvnPSRQyM8zRMv3BLvnPSTQCN831atUlNgM2boclakQlBMwDM3zSMv3BLvnPSRQCN8zRMv3BLvnPSTUSL87zTCQiNV8FaJzDS0DSOw.CMt.CLJzjT0DSOw.CLt.CLJzDU0HSOOM0PznCQzUmaJzDS0HSOv3BLvnPSRUiL8zhLz3BLvnPSTUyL87zTCQiNSYDVwnPSLUyL8.iKv.iBMIUMyzCM43BLvnPSTUCM831atUlNgM2boclakQlBMwTMzzSMv3BLvnPSRUCM8zRMv3BLvnPSTUSM831atUlNgM2boclakQlBMwTM0zSMv3BLvnPSRUSM8zRMv3BLvnPSTUiM831atUlNgM2boclakQlBMwTM1zSMv3BLvnPSRUiM8zRMv3BLvnPSTUyM831atUlNgM2boclakQlBMwTM2zSMv3BLvnPSRUyM8zRMv3BLvnPSTUCN831atUlNgM2boclakQlBMwTM3zSMv3BLvnPSRUCN8zRMv3BLvnPSTYSL83zaoMWYwniUuwlBMwjMwzSLv.iKv.iBMIkMwziLv.iKv.iBMQkMxzySSMTL5X0aroPSLYiL8.iKv.iBMIkMxzSKw.CLt.CLJzDU1LSOOM0PxniUuwlBMwjMyzCLt.CLJzjT1LSOsDCLv3BLvnPSTYCM831atUlNgM2boclakQlBMwjMzzSMv3BLvnPSRYCM8zRMv3BLvnPSTYSM831atUlNgM2boclakQlBMwjM0zSMv3BLvnPSRYSM8zRMv3BLvnPSTYiM831atUlNgM2boclakQlBMwjM1zSMv3BLvnPSRYiM8zRMv3BLvnPSTYyM831atUlNgM2boclakQlBMwjM2zSMv3BLvnPSRYyM8zRMv3BLvnPSTYCN831atUlNgM2boclakQlBMwjM3zSMv3BLvnPSRYCN8zRMv3BLvnPSTcSL8TjSVIiNAQ2ZJzDS2DSOyLiKv.iBMI0MwzSKwLiKv.iBMQ0MxzSQNYkL5PTYioPSLciL8DSMt.CLJzjT2HSOsHCMt.CLJzDU2LSOE4jUwnyT0MmBMwzMyzSLt.CLJzjT2LSOsLSNt.CLJzDU2PSOE4jUwnCQkMlBMwzMzzCLt.CLJzjT2PSOsTCLt.CLJzDU2TSOE4jUwniTkwlBMwzM0ziL23BLvnPSRcSM8zRLv3BLvnPSTciM831atUlNgM2boclakQlBMwzM1zSMv3BLvnPSRciM8zRMv3BLvnPSTcyM831atUlNgM2boclakQlBMwzM2zSMv3BLvnPSRcyM8zRMv3BLvnPSTcCN831atUlNgM2boclakQlBMwzM3zSMv3BLvnPSRcCN8zRMv3BLvnPSTgSL8PTYrEVdwnSSogmBMwDNwzCLt.CLJzjT3DSOsTCNt.CLJzDU3HSONUmTkYWL5zTZ3oPSLgiL8fiLt.CLJzjT3HSO2TiKv.iBMQENyziVME1b5zTXyQmBMwDNyziL43BLvnPSRgyL8HyLt.CLJzDU3PSONUmTkYWL5LUZ5UlBMwDNzzCLt.CLJzjT3PSOsDSL23BMwnPSTgSM83TcRUlcwnCQkMVX4oPSLgSM8DiKv.iBMIEN0zSKwbiKv.iBMQEN1ziS0IUY1EiNT8lakoPSLgiM8.iKv.iBMIEN1zyLx3BLvnPSTgyM831atUlNgM2boclakQlBMwDN2zSMv3BLvnPSRgyM8zRMv3BLvnPSTgCN831atUlNgM2boclakQlBMwDN3zSMv3BLvnPSRgCN8zRMv3BLvnPSMQUL831atUlNgM2boclakQlBM0zTwzCLJzTSDESOv3BLvnPSMY0TwzCLJzTSVQTL8.iKv.iBM0DUxziau4VY5D1byk1YtUFYJzTSSISOvnPSMQjL8.iKv.iBM0jUSISOvnPSMYEQxzCLt.CLJzTSTMSOt8lakoSXyMWZm4VYjoPSMM0L8.iBM0DQyzCLt.CLJzTSVM0L8.iBM0jUDMSOv3BLvnPSMQEM831atUlNgM2boclakQlBM0zTzzCLJzTSDQSOv3BLvnPSMY0TzzCLJzTSVQDM8.iKv.iBM0DU0ziau4VY5D1byk1YtUFYJzTSSUSOvnPSMQTM8.iKv.iBM0jUSUSOvnPSMYEQ0zCLt.CLJzTSTYSOt8lakoSXyMWZm4VYjoPSMMkM8.iBM0DQ1zCLt.CLJzTSVMkM8.iBM0jUDYSOv3BLvnPSMQ0M831atUlNgM2boclakQlBM0zT2zCLJzTSDcSOv3BLvnPSMY0T2zCLJzTSVQzM8.iKv.iBM0DU3ziau4VY5D1byk1YtUFYJzTSSgSOvnPSMQDN8.iKv.iBM0jUSgSOvnPSMYEQ3zCLt.CLJzTSTkSOt8lakoSXyMWZm4VYjoPSMMUN8.iBM0DQ4zCLt.CLJzTSVMUN8.iBM0jUDkSOv3BLvnPSMQULvziau4VY5D1byk1YtUFYJzTSSECL8.iBM0DQw.SOv3BLvnPSMY0Tw.SOvnPSMYEQw.SOv3BLvnPSMQULwziau4VY5D1byk1YtUFYJzTSSESL8.iBM0DQwDSOv3BLvnPSMY0TwDSOvnPSMYEQwDSOv3BLvnPSMQULxziau4VY5D1byk1YtUFYJzTSSEiL8.iBM0DQwHSOv3BLvnPSMY0TwHSOvnPSMYEQwHSOv3BLvnvTBE1bk0iLJL0co41Y8.iKv.iBSQkbocVOwn.TPI2ap0CLJ.kQuwFY8.iBPYTZrUVOwnvQFkFak0iLJbzTiEFak0CLJLDZLEVd8.iBSUmbx8TOvnfTkYWOwHCL4HiBLUDQ8.iKv.iBPEzQE0CLJ.UXmU1bO4VOvnvPuIWYN0yLJLEaoMVY8PiBUkzWuAWOznPSoQVZA0SMJzTZjkFT8XiBDYzarQVO2nvPzIGaA0iLJLDcxwlP8DSLJLxXs0CSF8zQJLUdtMVOsHiBTIWZm0CLJbUX1UVOwn.TnMWY8.iKv.iBREFck0CNv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWO3nPUWYWOvnvHi0VOLYzSGIiBSkmai0SKxn.Uxk1Y8.iBWElck0SLJ.EZyUVOv3BLvnfTgQWY8DCLv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWO4nPUWYWOvnvHi0VOVMzPJLBSF8TL8DiBivjQOISOwnvHLYzSyzSLJLBSF8DM8DiBV8VZiU1b8DiBV8VZiklam0CLJzzajUVOvn.TuIGcg0CLt.CLJ.kP8HiBPIDQ8HiBAI2Ti0iLJDjbOIGY8.iBAIGSv0CLJDjbOMFc8.iBAIGSL0SL1nPPxQkb8.iBDImYz0SLJzDU041T8.iBMQUct4TOw.iBMQUctQUOwDiBTI2bv0SKwHiBFQUct0CLt.CLJ.0axQmTm0SLv.iKv.iBP8lbzEVS8.iBP8lbzElL8zRLv.iKv.iBAcFckESOxnPPzIGbwzCLJDjcuMVL8DiBA0VcrESOwnPPs8FYwzCLJDTSDAGcwzCLt.CLJDTSDAmPwzCLt.CLJDzYzUlL8HiBAQmbvISOvnPP181XxzSLJDTa0wlL8DiBA01ajISOvnPPMQDbzISOv3BLvnPPMQDbBISOv3BLvnPPmQWYyziLJDDcxA2L8.iBAY2aiMSOwnPPsUGayzSLJDTauQ1L8.iBA0DQvQ2L8.iKv.iBA0DQvIzL8.iKv.iBAcFckQSOxnPPzIGbzzCLJDjcuMFM8DiBA0VcrQSOwnPPs8FYzzCLJDTSDAGczzCLt.CLJDTSDAmPzzCLt.CLJDzYzUVM8HiBAQmbvUSOvnPP181X0zSLJDTa0wVM8DiBA01ajUSOvnPPMQDbzUSOv3BLvnPPMQDbBUSOv3BLvnPPmQWY1ziLJDDcxAmM8.iBAY2aiYSOwnPPsUGa1zSLJDTauQlM8.iBA0DQvQmM8.iKv.iBA0DQvIjM8.iKv.iBAcFckcSOxnPPzIGb2zCLJDjcuM1M8DiBA0VcrcSOwnPPs8FY2zCLJDTSDAGc2zCLt.CLJDTSDAmP2zCLt.CLJDzYzUFN8HiBAQmbvgSOvnPP181X3zSLJDTa0wFN8DiBA01ajgSOvnPPMQDbzgSOv3BLvnPPMQDbBgSOv3BLvnPPmQWY4ziLJDDcxAWN8.iBAY2aikSOwnPPsUGa4zSLJDTauQVN8.iBA0DQvQWN8.iKv.iBA0DQvITN8.iKv.iBAcFckECL8HiBAQmbvECL8.iBAY2aiECL8DiBA0VcrECL8DiBA01ajECL8.iBA0DQvQWLvzCLt.CLJDTSDAmPw.SOv3BLvnPPmQWYwDSOxnPPzIGbwDSOvnPP181XwDSOwnPPsUGawDSOwnPPs8FYwDSOvnPPMQDbzESL8.iKv.iBA0DQvITLwzCLt.CLJDzYzUVLxziLJDDcxAWLxzCLJDjcuMVLxzSLJDTa0wVLxzSLJDTauQVLxzCLJDTSDAGcwHSOv3BLvnPPMQDbBEiL8.iKv.iBAcFckEyL8HiBAQmbvEyL8.iBAY2aiEyL8DiBA0VcrEyL8DiBA01ajEyL8.iBA0DQvQWLyzCLt.CLJDTSDAmPwLSOv3BLvnPPmQWYwPSOxnPPzIGbwPSOvnPP181XwPSOwnPPsUGawPSOwnPPs8FYwPSOvnPPMQDbzECM8.iKv.iBA0DQvITLzzCLt.CLJDzYzUVL0ziLJDDcxAWL0zCLJDjcuMVL0zSLJDTa0wVL0zSLJDTauQVL0zCLJDTSDAGcwTSOv3BLvnPPMQDbBESM8.iKv.iBAcFckEiM8HiBAQmbvEiM8.iBAY2aiEiM8DiBA0VcrEiM8DiBA01ajEiM8.iBA0DQvQWL1zCLt.CLJDTSDAmPwXSOv3BLvnvHi0VOE4jUwnPSuQVY8.iBo0zajUVOvnvbM8FYk0CLJjlaoQWOv3BLvnPPzsVOv3BLvn.QkMVOw.CLt.CLJLUcy0CMv3BLvnvT0MGU8.iKv.iBSU2bxzCLt.CLJHUYr0yLv3BLvnfUkwVOv3BLvnfUxjTOv3BLvnfUxDTOv3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVIiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8HCLt.CLJPTYi0iMv3BLvnvT0MWOv3BLvnvT0MGU8.iKv.iBSU2bxzCLt.CLJHUYr0SMv3BLvnfUkwVO4.iKv.iBVISR8.iKv.iBVISP8zhLz3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVMiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8.iKv.iBDU1X8.iKv.iBSU2b8PSNtTCLJLUcyQUOv3BLvnvT0MmL8.iKv.iBRUFa8DCLv3BLvnfUkwVOv3BLvnfUxjTOv3BLvnfUxDTOv3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVQiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8.iKv.iBDU1X8TCLt.CLJLUcy0SLv.iKv.iBSU2bT0CLt.CLJLUcyISOv3BLvnfTkwVOw.iKv.iBVUFa8.iKv.iBVISR8.iKv.iBVISP8.iKv.iBVICQ8.iKv.iBVIyT8.iKv.iBVIiQR0CLt.CLJXkLSISOv3BLvnfUxHUOv3BLvnvRxjTOv3BLvnvRxDTOv3BLvnvRxPTOv3BLvnvRxLUOv3BLvnvRxXjT8.iKv.iBKIyTxzCLt.CLJrjLR0CLt.CLJLEauAWY8zhMv3BLvn.UBE1bk0CLJLxXs0SSSUzQwn.UsUka8DiBE4lc8DiLJXUYr0CLt.CLJDDcq0CLt.CLJvDbz0CLt.CLJHUYr0CLt.CLJPkbocVOvnvHi0VOMMUQGIiBT0VUt0SLJTja10SLynfUkwVOv3BLvnPPzsVOv3BLvn.SvQWOv3BLvnfTkwVOv3BLvn.Uxk1Y8.iBiLVa8zzTEczLJPUaU4VOwnPQtYWOwPiBVUFa8.iKv.iBAQ2Z8.iKv.iBLAGc8.iKv.iBRUFa8.iKv.iBTIWZm0CLJLxXs0SSSUzQzn.UsUka8DiBE4lc8DSMJXUYr0CLt.CLJDDcq0CLt.CLJvDbz0CLt.CLJHUYr0CLt.CLJPkbocVOvnvHi0VOLYzSwnvT441X8zxLJPkbocVOwnvUgYWY8.iBPg1bk0CLt.CLJHUXzUVO3.iKv.iBA0Fb8DCLv3BLvnvTrU1c8DiBNMGcv0SL1nvTzA2b8DiMJT0U10CLJPDa40CLt.CLJPTSSESOwn.QMQTL8DCLv3BLvnfQMMUL8.iBF0DQwzCLt.CLJLxXs0CSF8jLJLUdtMVOsHiBTIWZm0SLJbUX1UVOwn.TnMWY8.iKv.iBREFck0SLv.iKv.iBA0Fb8DCLv3BLvnvTrU1c8DiBNMGcv0SL1nvTzA2b8DyMJT0U10CLJPDa40CLt.CLJPTSSESOvn.QMQTL8.iKv.iBF0zTwzCLJXTSDESOv3BLvnvHi0VOLYzSynvT441X8zhLJPkbocVOwnvUgYWY8DiBPg1bk0CLt.CLJHUXzUVOw.CLt.CLJDTav0SLv.iKv.iBSwVY20SLJ3zbzAWOwXiBSQGby0SL3nPUWYWOvn.QrkWOv3BLvn.QMMUL8.iBD0DQwzCLt.CLJXTSSESOvnfQMQTL8.iKv.iBiLVa8vjQOQiBSkmai0SKxn.Uxk1Y8DiBWElck0SLJ.EZyUVOv3BLvnfTgQWY8DCLv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWOwjiBUckc8.iBDwVd8.iKv.iBD0zTwzCLJPTSDESOv3BLvnfQMMUL8.iBF0DQwzCLt.CLJLxXs0SSMEFbwnPSuQVY8HiBMMkbi0SL1nvTzA2b8HCLJ3Tcs0yLJLxXs0SSMEFbxnPSuQVY8.iBMMkbi0CLJLEcvMWOxDiBNUWa8DyMJLxXs0SSMEFbynPSuQVY8LiBMMkbi0CLJLEcvMWOxHiBNUWa8DyMJLxXs0SSMEFbznPSuQVY8LiBMMkbi0CLJLEcvMWOxLiBNUWa8DyMJLxXs0SSMkFdwn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdxn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdyn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdzn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0yQxkFYJbjboQVOxPiBGITdv0CLJLxXs0ySSMTLJbUX1UVOvn.U04VY8.iKv.iBKUVdSMFa8DCLv3BLvn.UMMkbi0CLJPUSDAGc8.iKv.iBPg1bk0SMv3BLvn.TnMWSSI2X8.iBPg1bMQDbz0CLt.CLJbkS00VO03BLvnvUPMkbi0CLJbETDAGc8.iKv.iBVQ2aD0iM03BLvnvP0Imck0iL0n.TxU1X8TiKv.iBFgULTAWOvnvTFgUL8.iKv.iBFgULSMVOvnfQXECQz0CLt.CLJXDVxPEb8.iBSYDVxzCLt.CLJXDVxL0X8.iBFgkLDQWOv3BLvn.TuwVd8DiBDQWct0SK33BLvnvRVM2X8HiMJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SMv3BLvn.T201St0CLJbUXTIVOxbiBRUFTnMWOvnfSuIWa8DSMt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0iL3nvHi0VOOM0PxnvUgYWY8.iBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOxXiBT0DQvQWOwHiKv.iBPg1bk0SMv3BLvn.TnMWSSI2X8TiBPg1bMQDbz0iLx3BLvnvUNUWa8TiKv.iBWA0TxMVOvnvUPQDbz0CLt.CLJXEcuQTO0TiKv.iBCUmb1UVOxjiBPIWYi0SMt.CLJXDVwPEb8.iBSYDVwzCLt.CLJXDVwL0X8.iBFgULDQWOv3BLvnfQXICUv0CLJLkQXISOv3BLvnfQXIyTi0CLJXDVxPDc8.iKv.iBP8Fa40SLJPDc04VOz3BLvnvRVM2X8LCLJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SLv.iKv.iBPcWaO4VOwnvUgQkX8LSLJHUYPg1b8.iBN8lbs0SL03BLvnfTk4FY8.iBF0Fc5UVOvnvUT4TXsUVOyHiBiLVa87zTCMiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvn.TnMWY8TCLt.CLJ.EZy0zTxMVOvn.TnMWSDAGc8.iKv.iBW4Tcs0SMt.CLJbETSI2X8.iBWAEQvQWOv3BLvnfUz8FQ8.iKv.iBCUmb1UVOyLiBPIWYi0SMt.CLJXDVwPEb8.iBSYDVwzCLt.CLJXDVwL0X8.iBFgULDQWOv3BLvnfQXICUv0CLJLkQXISOv3BLvnfQXIyTi0CLJXDVxPDc8.iKv.iBP8Fa40CLJPDc04VOv3BLvnvRVM2X8LCMJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SMv3BLvn.T201St0CLJbUXTIVOyTiBRUFTnMWOvnfSuIWa8DSMt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0yL1nvHi0VOOM0PznvUgYWY8LiBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOvn.UMQDbz0CLt.CLJ.EZyUVOv3BLvn.TnMWSSI2X8.iBPg1bMQDbz0CLt.CLJbkS00VOw3BLvnvUPMkbi0CLJbETDAGc8DSMt.CLJXEcuQTO1TiKv.iBCUmb1UVOybiBPIWYi0yMt.CLJXDVwPEb8DiMJLkQXESOv3BLvnfQXEyTi0CLJXDVwPDc8.iKv.iBFgkLTAWOvnvTFgkL8.iKv.iBFgkLSMVOvnfQXICQz0CLt.CLJ.0arkWOxn.QzUma8HiKv.iBKY0bi0yL3nfUuwVOv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJLUdtMVOv3BLvnvTtM1Ti0CLJLkaiQDc8.iKv.iBS41XO4VOvn.Tuw1U8DCLv3BLvn.T201St0CLJbUXTIVOyjiBRUFTnMWOvnfSuIWa8HCLt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0CMvnvHi0VON8VZyUVLJPUdvUVOvnfQwzSMv3BLvnfQwLkbi0SL0nfQwPDbz0SLv.iKv.iBFISOv3BLvnfQxLkbi0CLJXjLDAGc8.iKv.iBKY0bi0CMwnfUuwVOv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvHi0VON8VZyUlLJPUdvUVOvnfQwzSLv.iKv.iBFEyTxMVOvnfQwPDbz0CLt.CLJXjL8.iKv.iBFIyTxMVOvnfQxPDbz0CLt.CLJrjUyMVOzHiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvHi0VOVMjQwn.U4AWOvnvP0QWOybiKv.iBRU1b8TiKv.iBDImc8.iKv.iBGEVZt0CLt.CLJXTSwzSLv.iKv.iBFMUL8DSMJXTSxziMv3BLvnfQSISOwDiBKUVdSMFa8PiLt.CLJLxXs0iUCYjLJPUdv0SL2nvP0QWO2fiKv.iBRU1b8LSMt.CLJPjb10iLv3BLvnvQgkla8.iKv.iBF0TL8LCNt.CLJXzTwzSMJXTSxzCLt.CLJXzTxzCLJrTY4M0Xr0CLt.CLJLxXs0iUCYzLJPUdv0SL3nvP0QWOwTCLt.CLJHUYy0CLt.CLJPjb10CLt.CLJbTXo4VOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnvHi0VOVMjQzn.U4AWOwfiBCUGc8DSMv3BLvnfTkMWOv3BLvn.QxYWOv3BLvnvQgkla8.iKv.iBF0TL8.iKv.iBFMUL8.iBF0jL8.iKv.iBFMkL8.iBKUVdSMFa8.iKv.iBiLVa8XTSOEiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzLiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOIiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzPiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOMiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzTiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOQiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzXiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8LzasIVLJzzajUVOvn.U04VY8.iKv.iBKUVdSMFa8DCLv3BLvn.UMMkbi0CLJPUSDAGc8.iKv.iBDUFct0CLt.CLJXEcuQTOv3BLvnfQB0CLt.CLJXjPSI2X8.iBFIDQvQWOv3BLvn.Qg0Fb8.iKv.iBD0FbSI2X8.iBD0FbDAGc8.iKv.iBEg2X8.iKv.iBI4lZ8DCLv3BLvnPRto1TxMVOvnPRtoFQvQWOv3BLvn.UtUVO0.iKv.iBT4VYSI2X8.iBT4VYDAGc8.iKv.iBSU1X8.iKv.iBSU1XSI2X8.iBSU1XDAGc8.iKv.iBDk1bz0CLt.CLJPjb40CLt.CLJX0ar0iLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvn.TuwVd8.iBP8FaW0SLv.iKv.iBFkFar0CLJLxXs0yPu0lXxnPSuQVY8.iBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOvn.UMQDbz0CLt.CLJPTYz4VOv3BLvnfUz8FQ8.iKv.iBFITOv3BLvnfQBMkbi0CLJXjPDAGc8.iKv.iBDEVav0CLt.CLJPTavMkbi0CLJPTavQDbz0CLt.CLJTDdi0CLt.CLJjjap0SLv.iKv.iBI4lZSI2X8.iBI4lZDAGc8.iKv.iBT4VY8TCLt.CLJPkakMkbi0CLJPkakQDbz0CLt.CLJLUYi0CLt.CLJLUYiMkbi0CLJLUYiQDbz0CLt.CLJPTZyQWOv3BLvn.QxkWOv3BLvnfUuwVOx.CLt.CLJX0arM0X8.iBV8FaDQWOv3BLvn.Tg4VOv3BLvn.Tg41Ti0CLJ.UXtQDc8.iKv.iBP8Fa40CLJ.0arcUOw.CLt.CLJXTZrwVOvnvHi0VOSgVXvUVLJPUdvUVOyn.QkAGcn0CLt.CLJPTSSI2X8.iBD0DQvQWOv3BLvnPQjcVY8DCLv3BLvnPQMMkbi0CLJTTSDAGc8.iKv.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.Ro8Tcz0CLt.CLJLxXs0yTnEFbkIiBTkGbk0yLJPTYvQGZ8.iKv.iBD0zTxMVOvn.QMQDbz0CLt.CLJTDYmUVOw.CLt.CLJTTSSI2X8.iBE0DQvQWOv3BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJfTZOUGc8.iKv.iBiLVa8zTZ3EiBPEla8.iKv.iBMkFd8TCLt.CLJ.kaMQVOvn.Tt0DQ8.iKv.iBP4VSS0CLJLxXs0SSogmLJ.UXt0CLt.CLJzTZ30SMv3BLvn.Tt0DY8.iBP4VSD0CLt.CLJ.kaMMUOvnvHi0VOMkFdyn.Tg4VOv3BLvnPSogWO0.iKv.iBP4VSj0CLJ.kaMQTOv3BLvn.Tt0zT8.iBiLVa8zTZ3QiBPEla8.iKv.iBMkFd8TCLt.CLJ.kaMQVOvn.Tt0DQ8.iKv.iBP4VSS0CLJLxXs0CVMYTLJPUdv0CLJLTcz0SL0.iKv.iBRU1b8.iKv.iBF0TL8.iKv.iBFMUL8.iBF0jL8.iKv.iBFMkL8.iBKUVdSMFa8.iKv.iBF8jYl0CLt.CLJXzSM8FY8.iKv.iBF8zTxMVOvn.VF0TOv3BLvn.VF0DQ8.iKv.iBXYTSS0CLJHTZgMWOv3BLvnvSL8VXj0CLt.CLJLDaoM1Z8.iKv.iBDImc8DiBR8Vcz0CLJPUdvISOsDiBiLVa8fUSFIiBTkGb8.iBCUGc8DSMv3BLvnfTkMWOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnfQOYlY8.iKv.iBF8TSuQVOv3BLvnfQOMkbi0CLJfkQM0CLt.CLJfkQMQTOv3BLvn.VF0zT8.iBBkVXy0CLt.CLJ7DSuEFY8.iKv.iBCwVZisVOv3BLvn.QxYWOwnfTuUGc8.iBTkGbxzSKwnvHi0VOSITLJHUXtcVY8.iBFIWYw0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnvSlY1b8.iKv.iBO0zTxMVOvnvSMQDbz0CLt.CLJzTZ30SMv3BLvnPSMMkbi0CLJzTSDAGc8.iKv.iBiLVa8LkPxnfTg41Yk0CLJXjbkEWOv3BLvnfQMMkbi0CLJXTSDAGc8.iKv.iBOYlYy0CLt.CLJ7TSSI2X8.iBO0DQvQWOv3BLvnPSogWO0.iKv.iBM0zTxMVOvnPSMQDbz0CLt.CLJLxXs0CQoMGcwn.U4AWY8.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.TxUFUowFc8.iKv.iBPMGcTkFaz0CLt.CLJLjazYjbkEWOw.CLt.CLJvza20CLt.CLJfTZmgVOv3BLvn.TuMGcFwFc8.iBiLVa8PTZyQmLJPUdvUVOvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJ.kbkQUZrQWOv3BLvn.TyQGUowFc8.iKv.iBC4FcFIWYw0SLv.iKv.iBL81c8.iKv.iBHk1Yn0CLt.CLJ.0ayQmQrQWOvnvHi0VOF8FajEiBRkFbvwVYy0CMJXzarQ1b8DCLt.CLJXTSuQVOv3BLvnfQSI2X8.iBBkVXy0CLt.CLJHTSuQVOv3BLvnfPSI2X8.iBFIUXzk1a8TCLt.CLJXzTr8Fbk0SLv3BLvnvPA41Yr0SMv3BLvn.SoYWY8.iBiLVa8XzarQlLJHUZvAGakMWOznfQuwFYy0SLv3BLvnfQM8FY8.iKv.iBFMkbi0CLJHTZgMWOv3BLvnfPM8FY8.iKv.iBBMkbi0CLJXjTgQWZu0SMv3BLvnfQSw1avUVOw.iKv.iBCEjamwVO0.iKv.iBLklck0CLJLxXs0iUCETLJ.UXtESOv3BLvn.Tg4VSSESOvn.Tg4VSDESOv3BLvnfUuwVL8TCLt.CLJX0PAESOwnPSuQ1TxMVL8.iBM8FYDAGcwzCLt.CLJ.UXtISOv3BLvn.Tg4VSSISOvn.Tg4VSDISOv3BLvnfUuwlL8.iKv.iBVMTPxzSLJzzajMkbiISOvnPSuQFQvQmL8.iKv.iBPElayzCLt.CLJ.UXt0zTyzCLJ.UXt0DQyzCLt.CLJX0arMSOxTiKv.iBVMTPyzSLJzzajMkbiMSOvnPSuQFQvQ2L8.iKv.iBPElazzCLt.CLJ.UXt0zTzzCLJ.UXt0DQzzCLt.CLJX0arQSOv3BLvnfUCEDM8DiBM8FYSI2XzzCLJzzajQDbzQSOv3BLvnPSTESOvnPSTISOvnPSTMSOvnPSTQSOvn.TBESOvn.TBISOvn.TBMSOvn.TBQSOvnfP0MWL8.iBBU2bxzCLJHTcyMSOvnfP0MGM8.iBSUlajESOv3BLvnvTtMkbiESOvnvTtQDbzESOv3BLvnvTk4FYxzCLt.CLJLkaSI2XxzCLJLkaDAGcxzCLt.CLJDDczMUOwnvHi0VOGIWZjYDVJbjboQVOzbiBGITdv0CLJLxXs0SSuQlQXEiBM8FYk0CLJLTYtQWOwbiK0.iBSAWYj0SMv3BLvn.Tn8jYl0SMv3BLvn.QvQGZ8jiLtTCLJXTYkITOv3BLvnPSogWOxTiKv.iBLMTcz0CLt.CLJfzP0QWOw.CLt.CLJDUcgQVO1.iKv.iBQAGZy0iL03BLvn.SkEWOv3BLvn.RkEWOv3BLvnPTwzCLt.CLJDkL8.iKv.iBEE0at0CLJLxXs0SSuQlQXIiBM8FYk0iLJLTYtQWOz.iKv.iBSAWYj0SMt.CLJ.EZOYlY8TCLt.CLJPDbzgVOz.iKv.iBFUVYB0SMv3BLvnPSogWO0.iKv.iBLMTcz0CLt.CLJfzP0QWOw.CLt.CLJDUcgQVOv3BLvnPTvg1b8HSMt.CLJvTYw0CLt.CLJfTYw0CLt.CLJDUL8.iKv.iBQISOv3BLvnPQQ8la8DiBiLVa8PTYrEVdwnPSuQVY8HiBMkFd8LyLt.CLJXjP8.iKv.iBCITO1TiKv.iBLAUO2.iKv.iBHAUO0.iKv.iBDImc8.iKv.iBSkmaiESOznvT441XxzSNJLUdtM1L8PiBSkmaiQSOzn.UvzSLv.iKv.iBTESOw.CLt.CLJPkL8DCLv3BLvn.UyzSLv.iKv.iBPElawzSKw.CLt.CLJ.UXtISOw.CLt.CLJ.UXtMSOsDCLv3BLvn.Tg4FM8DCLv3BLvnvHi0VODUFagkmLJzzajUVOvnPSogWO0.iKv.iBFITOz.iKv.iBCITOv3BLvn.SP0yMv3BLvn.RP0yLv3BLvn.QxYWOv3BLvnvT441XwzSKwnvT441XxzSKwnvT441XyzSKwnvT441XzzSKwn.UvzCM03BLvn.UwzyLv3BLvn.UxzSLv.iKv.iBTMSOw.CLt.CLJ.UXtESOsDCLv3BLvn.Tg4lL8DCLv3BLvn.Tg41L8zRLv.iKv.iBPElazzSLv.iKv.iBiLVa8LEZgAWYyn.U4AWY8LiBDUFbzgVOv3BLvn.QMMkbi0CLJPTSDAGc8.iKv.iBEQ1Yk0SLv.iKv.iBE0zTxMVOvnPQMQDbz0CLt.CLJjjavUGc8.iKv.iBOUGcvUGc8.iKv.iBHk1S0QWOv3BLvnvHi0VOSgVXvUFMJPUdvUVOyn.QkAGcn0CLt.CLJPTSSI2X8.iBD0DQvQWOv3BLvnPQjcVY8DCLv3BLvnPQMMkbi0CLJTTSDAGc8.iKv.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.Ro8Tcz0CLt.CLJLxXs0SSogWMJ.UXt0CLt.CLJzTZ30SMv3BLvn.Tt0DY8.iBP4VSD0CLt.CLJ.kaMMUOvnvHi0VOMkFd1n.Tg4VOv3BLvnPSogWO0.iKv.iBP4VSj0CLJ.kaMQTOv3BLvn.Tt0zT8.iBiLVa8HUY1EiBM8FYk0CLJPjb40SLv.iKv.iBWUFc8TCLt.CLJXjP8bCLtTCLJPTXsAWOxLiKv.iBSkldk0yM03BLvnvTvQVO0.iKv.iBDAGc8TSMt.CLJPjQB0yMv3BLvn.QSkldk0yMy3BLvnPQMkFd8DCLv3BLvn.QM8FY8TSMt.CLJPzTvQVO0TiKv.iBPIWY8HCLt.CLJLxXs0yPu0Fbwn.U4AWY8DiBREFc8PCLt.CLJPEZxU1b8.iKv.iBAQGc8HCLt.CLJHUYr0iL03BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJLxXs0yPu0Fbxn.U4AWY8DiBREFc8PCLt.CLJPEZxU1b8.iKv.iBAQGc8HCLt.CLJHUYr0iL03BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJLxXs0SQQEiBlMVL8HCLt.CLJHWYyESOxTiKv.iBmEVZtESOv3BLvnfYiISOz.iKv.iBxU1bxziL03BLvnvYgklaxzCLt.CLJX1XyziMv3BLvnfbkM2L8HSMt.CLJbVXo41L8.iKv.iBlMFM8fCLt.CLJHWYyQSOxTiKv.iBmEVZtQSOv3BLvnvHi0VOEEkLJX1XwziLv3BLvnfbkMWL8HSMt.CLJbVXo4VL8.iKv.iBlMlL8PCLt.CLJHWYyISOxTiKv.iBmEVZtISOv3BLvnfYiMSO1.iKv.iBxU1byziL03BLvnvYgklayzCLt.CLJX1XzzCNv3BLvnfbkMGM8HSMt.CLJbVXo4FM8.iKv.iBiLVa8X0PFUiBTkGb8DCNJLTcz0SL0.iKv.iBRU1b8.iKv.iBDImc8.iKv.iBGEVZt0CLt.CLJXTSwzCLt.CLJXzTwzCLJXTSxzCLt.CLJXzTxzCLJrTY4M0Xr0CLt.CLJLxXs0iUCYjMJPUdv0SL3nvP0QWOwTCLt.CLJHUYy0CLt.CLJPjb10CLt.CLJbTXo4VOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnvHi0VOSIzLJHUXtcVY8.iBFIWYw0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnvSlY1b8.iKv.iBO0zTxMVOvnvSMQDbz0CLt.CLJzTZ30SMv3BLvnPSMMkbi0CLJzTSDAGc8.iKv.iBiLVa8fUSFMiBTkGb8.iBCUGc8DSMv3BLvnfTkMWOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnfQOYlY8.iKv.iBF8TSuQVOv3BLvnfQOMkbi0CLJfkQM0CLt.CLJfkQMQTOv3BLvn.VF0zT8.iBBkVXy0CLt.CLJ7DSuEFY8.iKv.iBCwVZisVOv3BLvn.QxYWOwnfTuUGc8.iBTkGbxzSKwnvHi0VODk1bzMiBTkGbk0CLJjjavUGc8.iKv.iBOUGcvUGc8.iKv.iBPIWYTkFaz0CLt.CLJ.0bzQUZrQWOv3BLvnvPtQmQxUVb8DCLv3BLvn.SucWOv3BLvn.RocFZ8.iKv.iBP81bzYDaz0CLJLxXs0CQoMGczn.U4AWY8.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.TxUFUowFc8.iKv.iBPMGcTkFaz0CLt.CLJLjazYjbkEWOw.CLt.CLJvza20CLt.CLJfTZmgVOv3BLvn.TuMGcFwFc8.iBiLVa83TcRUlcwn.TxUVOv3BLvn.Qg0Fb8fSLtTCLJPTYiEVd8byLtTCLJLUZ5UVOwLiLt.CLJP0atUVOsLiMt.CLJbUZjQGZ8XSNtTCLJzTZ30CLt.CLJLxXs0iVME1bJHUYzESOv3BLvnfTkQmL8.iKv.iBME1bz0yMv3BLvn.VYECS8PCNJfUVxvTOzjiBXk0LL0SMvn.VYQCS8TSLJfUVwPUO0HiBXkkLT0SMyn.VYMCU8TCMJfUVzPUO0TiBOM0PwzSM1nvSSMjL8TyMJ7zTCMSO0fiBOM0PzzSM4nPSSUzQwziMvnPSSUzQxziMwnPSSUzQyziMxnPSSUzQzziMynPSSUzQ0ziMznPSSUzQ1ziM0nPSSUzQ2ziM1nPSSUzQ3ziM2nfTkYWL8XCNJ.kayzCLJ.kazzCLJ.ka0zCLJ.ka1zCLJ.ka2zCLJ.ka3zCLJ.ka4zCLJ.kaw.SOvn.TtESL8.iBRE1XqASO1jiBRE1XqESO2.iBiLVa8vTX40TYso.SgkWSk0VL8.iBLEVdMUVaxzCLJvTX40TYsMSOvn.SgkWSk0FM8.iBLEVdMUVa0zCLJnfBJnvKu.xTkMFco8lafX1axARcmwVdfL1asAmbkM2bkQFHhklagIWdfPTXzElBu7BHD8jSmPEHT8TUCgDHTgTRSofBjPBIjbyMyPCLJ7SXgEVXoEFYvoiZqolZpoFauoSXgEVXoEFavoyYjc1Xk4VYhoCbtAGbnAGYvoCbsAGbnAGYvoyZqs1ZqsFauoiBq01Zqs1Zj8lNgEVXgEVXkElNiEVXgEVXgElNgElNjAmNrAmNj8lNkIlNkMlNr8lNmYlNkElNmIlNnAmNmAmNnovZ5LVX5fVY5vla5Pla5.Gb5D1Z5P1X5XFY5blZ5HVX5jVX5PVX5TlagPWPgIVPw.SYswzYjozYsMFbjUlSlElUJnzYuwjUmUFZjEjLyfCcIETLvX0YuY0SVozYsYUSHM1amkFUnEVPlEFSnQlUOYESm81XucVZTgVXAMiMzkTP2PiBzkTPw.SXjUVaUkVZp0VXkolYh8lavklZp81XroVaVwlZiYlag0VXVEVYwrFZpcVXv0VZgUFZhIFYu01Yo4Vauo.brMlTl0VSgglXo4VYoMlXlMVan8VYnsFamQ1YroVXoUFag4VaoklYgw1Zuc1apM1XqslXq4VYpAGagA2XjgFcJDVYAECLpE1ZsYlaSQWPgIVP4TVZLgFZJg1XmUlSUcFYJg1XwDjLzLCcXETLyfCcAElXAIiM0PGVAYSbAEiL2PmBXEjMwETLxbCcIETLvbVYHc1YJglYm01SNgFYmQlRm0FRAIiLwkWPwjTLAECVIETLqEVRAESagkTPw7VXIQWPgovXAUSbAMSb4ETLIESPwfURAEyZgkTPwzVXIETLuEVRAISQAEyUEETLNUTPwjUQAESREETLlEVQAEyYgUTPwfVXJTTPwfUQAESZoUTPwnVXEETLpkVQAEyZgUTPwrVZEETLrEVQAECaoUTPwzVXEETLskVQAEiagUTPw3VZEETLuElBEETLukVQAECbgUTPw.WZEEjLFETLgUlQAESXoYTPwDVaFETLWYTPwHVYFETLhklQAEiXsYTPw3jQAEyXkYTPwnvXoYTPwLVaFETLYYTPwPVYFETLjklQAECYsYTPwjjQAESYkYTPwTVZFETLk0lQAEiYgYTPwXVYFETLlklQAEiYJzlQAEyYgYTPwbVYFETLmklQAEyYsYTPwfVXFETLOYTPwfVZFETLn0lQAECVFETLoMlQAESZkYTPwj1YFETLoklBFETLoslQAESZsYTPwj1aFETLpElQAEiZiYTPwnVYFETLpclQAEiZoYTPwn1ZFETLp0lQAEiZuYTPwrVXFETLqovXFETLqUlQAEyZmYTPwrVZFETLqslQAEyZsYTPwr1aFETLrElQAECaiYTPwvVYFETLrclQAECaoYTPwv1ZFETLJvVaFETLGYTPwzVXFETLsMlQAESakYTPwz1YFETLsklQAESaqYTPwzVaFETLs8lQAEiagYTPw31XFETLtUlQAEiBtclQAEiaoYTPw31ZFETLt0lQAEiauYTPw7VXFETLuMlQAEyakYTPw71YFETLuklQAEyaqYTPw7VaFETLu8lQAoPLvElQAECbiYTPw.WYFETLvclQAECboYTPw.2ZFETLv0lQAECbuYDcYE1XAUSbAMSXhEjLgIVPxDlXAMSXkEjLJTibwEzMx41YpwlYuQTP2HWPwjTLAcibAECVIEzMxETLqEVRAcibAESagkTP2HWPw7VXIEzMxEjLEEzMxETLWUjBAcibAEiSEEzMxETLYUTP2HWPwjTQAcibAEiYgUTP2HWPwbVXEEzMxETLnEVQAcibAECVEEzMxETLokVQAcibAoPLpEVQAcibAEiZoUTP2HWPwrVXEEzMxETLqkVQAcibAECagUTP2HWPwvVZEEzMxETLsEVQAcibAESaoUTP2HWPJDiagUTP2HWPw3VZEEzMxETLuEVQAcibAEyaoUTP2HWPw.WXEEzMxETLvkVQAcibAESMzkUXiETMwEzLgIVPxDlBhEjLgIVPyDVYAISMxEWP2HWdAcibAESRwDzMxETLXkTP2HWPwrVXIEzMxETLsEVRAcibAEyagkTP2HWPxTTP2nfbAEyUEEzMxETLNUTP2HWPwjUQAcibAESREEzMxETLlEVQAcibAEyYgUTP2HWPwfVXEEzMxETLXUTP2HWPwjVZJTTP2HWPwnVXEEzMxETLpkVQAcibAEyZgUTP2HWPwrVZEEzMxETLrEVQAcibAECaoUTP2HWPwzVXEEzMxETLsklBEEzMxETLtEVQAcibAEiaoUTP2HWPw7VXEEzMxETLukVQAcibAECbgUTP2HWPw.WZEEzMxETL0PWVgMVP0DWPynPXhEjLgIVPxDlXAMSXkEjL0HWbAcib4EzMxETLIESP2HWPwfURAcibAEyZgkTP2HWPwzVXIEzMxETLuEVRAcibJDjLEEzMxETLWUTP2HWPw3TQAcibAESVEEzMxETLIUTP2HWPwXVXEEzMxETLmEVQAcibAECZgUTP2HWPwfUQAciBxQlZPklXEEzMxETLpEVQAcibAEiZoUTP2HWPwrVXEEzMxETLqkVQAcibAECagUTP2HWPwvVZEEzMxETLsEVQAovMxETLskVQAcibAEiagUTP2HWPw3VZEEzMxETLuEVQAcibAEyaoUTP2HWPw.WXEEzMxETLvkVQAcibAESMzkUXJLVP0DWPyDlXAISXhEjLgIVPyDVYAISMxEWP2HWdAcibAESRwDzMxETLXkTP2HWPwzVXIEzMxETLNUTP2HWPwbkBEEzMxEjLEEzMxETLuEVRAcibAEyZgkTP2HWPwjUQAcibAESREEzMxETLlEVQAcibAEyYgUTP2HWPwfVXEEzMxoPPwfUQAcibAESZoUTP2HWPwnVXEEzMxETLpkVQAcibAEyZgUTP2HWPwrVZEEzMxETLrEVQAcibAECaoUTP2HWPJDSagUTP2HWPwzVZEEzMxETLtEVQAcibAEiaoUTP2HWPw7VXEEzMxETLukVQAcibAECbgUTP2HWPw.WZEEzMxEjBwTCcXEjMwETLxbCcXEjMwETLxbCcXEjMwETLxbCcXEjMwETLxbCcAE1XAUSbyY1YlYlalcTPyX1YlYlXlITb2o.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.YmYFZC4VXso1YuMTagwjZgEkSFsFYPEVZioFarQTag4lZo8FTXkDarE0ZswVZic1PmUVYuE1YCgkYnY1XDcVXJ7FakA2QtklYgAWaDkERmU1QuEFarMVZDQWXsEFYAUSbkUVYpY1XkgVXkEjLg0VPxTFbUUFYjIVPJglZHElahIlBAEiYooUYmQlXAoDZpgzTnIVPwTFbUUFYjUVPNQlXjkVXtIVYAESUmklRnEFRTETLSIlXAESYvUUYjQUPyDlahofXAEiVVgVZjIVPyLEZiETLlklVkcFUAMyTnIVPwT1YZUFbjUVPyD1ahIVPwT1aLYEZjgDYhETLg4lXhETLk8FSJXEZjgDUAESXuI1XAESUFQlXAQyThMVPwX1YkQVYmQFYAoDZpgTXiIlXAEiYmUFYkcFYhEzLgIlXhETLkclVkAmBjQVPyD1ahIVPwT0YooDZggDYhEDRSI1XAEiVVgVZjUVPyLEZiETLlMlUm81YnQlXAIyTOETLkclVkAGUA4DUjofXg8lXhETLkUlUnQ1STEDZpgzThMVPwnkUnkFUA4DUjU1TnkVPwnkUnkFYjEzLSglXAESYjwzYtc1XjIVPxLkXJLVPwTFYLclamMFUAIyThMVPwX1XVc1amgFUAIyTnkVPwX1YkQVYmQVYAMSXiIlXAEiYmUFYkcFUAMSXhESPwTlBmoUYvQlXAMSXuIlXAESUFQUPnMlSTQFZSIVZAESYmwzYscVYjIVPxLkXkETLkcFSm01YkQUPxLkXoEjLNQEYooPPxLSYsoDZpgTPxfiRnoFRnMlSjQVVAMSYvUUYjQFYAMSXuIlXAECMNQFYjIVPwjSYkYEZj8DYhEjLSIlXAISNJnWPxPyLzEVY5EjL4EzLvoVURQTbxDTLwDCam01aoQFQREyRBEWLAESLwT1ZU4VYQA2aRsjPwESPwDSLt81XnMlBiEUXisVaWITRvg1UBEWPwDCNBAmalYVPBEWPwDSMv8VURQDboIULDEWPwDSMvw1ZnA2aDAGZUIEQwETLwTCbgofVuQFQuwFbp8FYDEWPwDSMmc1avw1ZDcVXpwFarQTbAESL0HVZl8VZoQjXjMUZpQTbAESL0nlYpo1XrQTZrAmXJLVaDEWPwDSMSUVZgcFQAsVXggFQwETLwTCZhoFZrQVTlwVYnw1YQEWPwDSMo8lXjYlZQcFYnQlYuEUbAESL0zlBg4VYusFYscVXpUFblQVawETLvfSdPc1XjwFagE1Xi0FYsoVYYUFbj0VbAECL2T2bvklTKITbwDTLvbScyYWbwnPPw.yM0MWbxDTLvbScywVYhcFaiwFakcVZt0VYjwVbAECL2T2bAMCUhUlYmQVawETLvbScyclaM4FYG4Dbk8lXJPTbAECL2T2bv4lTKMjcwETLvbScyAmaRszP1EWPw.yM0MGbtI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRsjBCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScv0lTKovPyYWbAESLwTVauIFbqczbAESL43VXs0lZscTZk8FalI1QAESL4TVauIFbqAUPwf0QhI1ZuUFZGETLwTSYs8lXJzDTAIyQAEiM0DiaksFYYIzag0jZkQTPwDSNqEFZgIlYBoVZpoFYpQTPwDSNtkVSvUFQs4VasEVaDETLwjiao0jBOQzag0jXkEUPwXSM0DlYAICcWEjM4YVZk0FYsMVYKMyYgUVZYIVZKMCcAoWPxjWP3DSNwPmdAciYqQkSlglRnovYH8jRmM1YsgjSkYFZogVXLg1XOETLxPWXkoWPxjWPy.mZUIEQwISPwDSLrcVaukFYDIULKITbwDTLwDSYqUkaJTVTv8lTKITbwDTLwDiauMFZiMVTgM1ZsckPIAGZWITbAESL3HDbtYlYAITbAESL0.2aUIEQvklTwPTbAESL0.mBrsFZv8FQvgVURQTbAESL0.WXZ8FYD8Favo1ajQTbAESL0b1YuAGaqQzYgoFarwFQwETLwTiXoY1aokFQhQ1ToofZDEWPwDSMpYlZpMFaDkFavI1XsQTbAESL0LUYoE1YDEzZgEFZDEWPwDSMnIlZnwFYQYFakgFamEUbAESL0j1aJHFYloVTmQFZjY1aQEWPwDSMsElak81Zj01YgoVYvYFYsEWPw.CN4A0YiQFarEVXiMVaj0lZkkUYvQVawETLvbiB0MGboI0RBEWLAECL2T2b1EWLAECL2T2bwISPw.yM0MGakI1YrMFarU1Yo4VakQFawETLvbScyEzLTIVYlcFYsoPbAECL2T2bm4VStQ1QNAWYuIFQwETLvbScyAmaRszP1EWPw.yM0MGbtI0RCYWbAECL2T2bv4lTKMjcwETLvbScJLGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbiB0MGbsI0RCYWbAECL2TGbsI0RCMmcwETLwDSYs8lXvs1QyETLwjiag0Vap01QoU1arYlXGETLwjSYs8lXvsFTAoPLXcjXhs1akg1QAESL0TVauIVSPEjLGETL1TSLtU1ZjkkPuEVSpUFQAESL4rVXnElXlIjZoolZjoFQAESL43VZJzDbkQTat0Vag0FQAESL43VZM8DQuEVShUVTAEiM0TSXlEjLzcUP1jmYoUVaj01XkszLmEVYokkXoszLzEjdAIiB4EDNwjSLzoWP2X1ZT4jYnoDZmgzSJc1Xm0FRNUlYnkFZgwDZi8TPzfFaWETMzEVY5EjL4EzLvoVURQTbxDTLwnPLrcVaukFYDIULKITbwDTLwDSYqUkakEEbuI0RBEWLAESLw31aig1XiEUXisVaWITRvg1UBEWPwDCNBAmalYVPJHTbAESL0.2aUIEQvklTwPTbAESL0.GaqgFbuQDbnUkTDEWPwDSMvElVuQFQuwFbp8FYDEWPwDSMmc1avw1ZDclBgoFarwFQwETLwTiXoY1aokFQhQ1TooFQwETLwTiZlolZiwFQowFbhMVaDEWPwDSMSUVZgcFQAsVXggFQwETLwnPMnIlZnwFYQYFakgFamEUbAESL0j1ahQlYpE0YjgFYl8VTwETLwTSag4VYusFYscVXpUFblQVawETLvfSdPc1XJPFarEVXiMVaj0lZkkUYvQVawETLvbScyAWZRsjPwESPw.yM0MmcwESPw.yM0MWbxDTLvbScywVYhcFaiwFakclBo4VakQFawETLvbScyEzLTIVYlcFYsEWPw.yM0M2Yt0jajcjSvU1ahQTbAECL2T2bv4lTKMjcwETLvbScyAmaRovRCYWbAECL2T2bv4lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaJH0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0AWaRszPyYWbAESLwTVauIFbqczbAESL43VXs0lBp01QoU1arYlXGETLwjSYs8lXvsFTAECVGIlXq8VYncTPwDSMk01ah0DTAIyQAEiM0DiaksFYYIzag0jZkQTPwnPL4rVXnElXlIjZoolZjoFQAESL43VZMAWYD0las0VXsQTPwDSNtkVSOQzag0jXkEUPwXSM0DlYAICcWEjM4YVZJTVaj01XkszLmEVYokkXoszLzEjdAISdAgSL4DCc5EzMlsFUNYFZJg1YH8jRmM1YsgjSkYFZogVXLg1XOEDMqklBh0VP0PWXkoWPxjWPynFaqM1XmQTPxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxnfPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLwTSXlI1YrMFYsEzLJDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMiBwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAQiLvbSXkoPPxP2UAYSdlkVYsQVaiU1RybVXkkVVhk1RyPWP5EjL4EzLwEzMwETL43FYpwlYuITPwDSStQlXrQTPy.0apokPJDTLwvTSUIjajoFat8VTAcyaqoESBEzMiIVXrYlZBEjL2DWPxbiZs41aOITPzbCaioVXgYlPrMlZgklYQUFYhclBrMFasEzL0DWPyDiXmw1XpEFQAESLkQlXmw1XQETL0zjajIFaDUFYhcFaiQVaAEiMy.0apo0PAcyaqoESCETLwn.aioVXgY1QAMSStQlXrMTPwjSaoYjXmMTPxLyXhEFalo1PAIiLCETL0vTSUMTPwDCSM4FYGEjLyLlXgwlYpMTPJHyLrMlZgklYGETLwbVYiIVXrMTPwjSXrYlZkk1PAESMp0lau8zPAIyMtAGbks1YGETL4DFaloVaocTPyDSYjIlBmQ0QAESM4vTSUITP2H1YrM1UBETLwT1aL0jPAESMloVaoYjPAMSMgwlYpUVZBEzL0DFaloVYoITPyTCSMUkPAovLwrFZjgVTBEjL2.mYqcFYnITPxbSMuslVLMTPwDiajoFal81PAESMoc1XscVYCETL0nVat81SCEzL4nVat81SJLTPxbiZs41avU1QAIyLM4FYpw1QAUSLM4FYhw1PAESMloVaoYzPAISM3HTPwDiXmw1XpEFQAcCSMUkPAIyLtQlBpwlYuITPwTiajoFal8lPAMSNtQlZrY1aBETLwPVZPYkPAMSMloVaoYjPAESLmU1XhEFaBEzLwnVat81SBEjL2nPNrMlZgklYGETLwPVZPY0PAESLM4FYhw1PAMSMpIVZlMVaCEzL0H1YrM1UCEzLhcFaic0PAciXmw1XWMTPwTyaJrlVLMTPy.yPAESLmU1XhEFaCETLwH1YrMlZgcTPzbCbls1Yjg1PAIyL4rFZjgFTDEzLkQlXmQUTL0TUBETL47lBqoESBEzL4zjajIFaBEjLyzVZFI1YBEjLyT1aL0jPAIyLM4FYhwlPAESNM4FYhwlPAciavAWYiclPAESNsklQhovYBEjL3LiajoFal81QjkFTuo1QAIyLkQlXmQ0PAESNyETLw3FbvU1XmMTPwDybAMSMmU1XhEFaCEzLyEzMmU1XJHVXrMTPyjiZhklYi01PAMSL0nVat81SDEjL271ZZwjPAESNtQlZr41aDEzLhcFaickPAESMiIVXrYlZBETL43lBjoFat8FQAESNjkFTVITPwjiZs41avUFQAESNuslVLITPwjSaoYjXmITPwjCYoAkUBEjL3LCTuolVCEzMrMlZgoPZlcjZs41aOcTPynVat81SCw1XpEVXlcTPwDSXrYlZkk1PAESMqgFYnE0PAIyLP8lZZMTPwjiXmw1XWMTPwTSXJvlYpUVZCETL0vTStQ1QAESNP8lZZMTPwjCaioVXoY1QAESLmU1XhEFaCETL4DFaloVYoMTPwTiZs41aOMTPxbiBtAGbks1YGETL4DFaloVaocTPyDSYjI1YTcTPwTSLi41YkMlXBEzLL0TUBEzMp0lau8jPL0jajQTPyzjajoFaDoPPwjiYp0VZFITP2DWPxLSXrYlZkklPAcyYkMlXgwlPAIyLgwlYpUVZBEzL0vTSUITPyDyZnQFZQITPxbCbls1YJPFZBEjL1bSaoYjXmMTPxLiZs41avU1QAMCYoAkUCEjLyzjajIFaCETL43FYpwlYuMTPwTyYkMlXgw1PAMybAciBrMlZgElYCEzMyEzMmU1XhEFaCEzL0TiZs41aOQDSMUkPAciXmw1XWITPyPVZPYkPAMSYuwTSBETL0XlZsklQBoPPxbSbAQyLgwlYpUVZBEzMhcFaickPAMCYoA0apQTPwDSbAcSaoYjXmITPyvTStQFQAESMqgFYnEkPAESNuslVJvjPAIyM0TFYhcFaicTPy71ZZwzPAESLtQlZrY1aCETL0j1Yi01YkMTP2.mYqcFYnMTPynVat81SCETLwLWPxLiBp0lau8zPAIyMp0lauAWYGEjLyzjajoFaGETMwzjajIFaCETL0XlZsklQCEjL0fiPAESMtQlZrY1aBEzMwEjLynPbAESLtQlZrY1aBEzL43FYpwlYuITPwDCYoAkUBEjL2nVat81SBETLzHTPwTSYuwTSBEzMtQlZrY1aBEzMmU1XJHVXrITP2nlXoY1XsITPxbSLp0lau8zPAcSYuwTSCEzMp0lau8zPAcybAcSYuwTSC4FYpwlauAUPyLWPwDybAEiBwLWP2LWP2T1aL0zPAESMyEzLyETL0LWPwDybAMSMvY1ZmQFZCEDM2LWPxbiXmw1XWMTPwjSNzoWP2X1ZT4jYnofRncFROozYicVaH4TYlgVZnEFSnM1SAQSarIlYAUCcWEjM4Y1XlIVYlEzRzf1aIIzRyP2UAYSdlMlYhUlYAsDMJf1aIIzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DjBKMCcg0VXjETMwUVYkolYiUFZgQVPxD1YAISYkgzYsoDZpQlXAEyThIVPwTFYLclanEFUAIyThMVPwnESmUVYmofYoQlXAQ0ThIVPwTVYVgFYOQFYAQUPSIVYAESYuglYlMFRncFYhETLgolXhcUPUcVZJgVXHQVYAEyThIVPwT0YJjlRnEFRjQVPwLkXhETLkUlUnQ1SjUVPxLkXoETLZYEZoQlYAoDZpgzTnIVPwnkUnkFYmEzLSg1XAESYkgzYsojBnoFUAEyThIVPwX1XHg1YjIVPyLkXhcUPlMlUm81YnQFYAIyTnIVPwnESmUVYmYVZTETLSIlXAESYjwzYtgVXjofXAIyThIVPwLiYmUFYkcFYlEzLgMlXhETLkYlYhQUPzLkXiETLkYlYhQlXAUVaJglZHMkXhEjLJglZHg1XNQVZJDDMlcVYjU1YjcVPyD1XhIVPwTkQjQVPzLkXiETLlklVkcFYjEzLSg1XAEyMk0lRnoFRAICNJglZHg1XNQlXwDjBxfiSjIFUAIyLk0lRnoFRAICNJglZHg1XNQlXjUVPxfiSjIFYlEjLyTVaJglZHEjL3nDZpgDZi4DYhQFZAICNNo.YhQVZAIyLk0lRnoFRAICNJglZHg1XNQUVAICNNQEYhEjLyTVaJglZHEjL3nDZpgDZi4DUjQVPxfiSTQVYAESNJPmdAcSYvgFYmQlUm0VLJ8DSnMFZjEjLvPmdAcSYmY0Ys8DRnMVPxTCc5EzMZYEZo8DZlg1XHEjLzPmdAcSYlglBo8DZioDZjEjL0PWPgIVP4XVZQU1YVc1aH4jYkglYm8FRSYlZQUlZm81SHg1XnclRm0VPxLyLzETXhETNlkVTko.YnY1SLc1YwLkYpEkYigDZjwzYuozYucFYHEjLyTCcAElXAkiYoEkQHcVawLkYpEUYuwjUnQFRAICMwPWPgIVPJjiYoEUYlc1ancFRm0FSnEFRSYlZQUEZgozYjgDZjEjLyXCc5EzMkclUm81XtUkRng1XtYVXm0FZjMlaUglXnYlBAcCaggVZiwlXnElXAIiZsQmdAcSYmY0YuMlaUoDZnMlalE1YsgFYi4VUnIFZlEzMrEFZoMFahgVXhEjLp0Fc5oPP2T1YVc1ai4VUJgFZi4lYgcVanQ1XtUEZhglYAcCaggVZiwlXnElXAIiZsQmdAciQHcVawfFYNU1Ym0lUnEVLJfDZiETL2DlXzoWP2bVYHc1YJglYm01SAEiMkY1Yug1YTEzLzoWP2bVYHc1YJglYm01SAEiMkY1Yug1YjQVPyPmB5EzMmUFRmclRnY1Ys8TPwXSYlc1ancFYkEzLzoWP2bVYHc1YJglYm01SAEiMZUUYlUFZjIVPxPmdAgCRmclRnofYm01SAQFYvk1YgMDamg1XmE1Pn81atYFbCklZmklYvMjap8FYl81Pm4lYvY1aCQmdAgCRmclRnY1Ys8TPrwVZJXVVBk1YnQVVBYVXJkkPhkVYvkkPt8FYskkPqM1XqkkPzoWP3fzYmoDZlcVaOETYuglXoclPCkFaoclPjY1ZmklBmIDah0VXoclPT4FaoclProFblk1YBQmdAgCRmclRnY1Ys8TPYAGakkFTnQlXuUVaPsFbEUFbP81XmUlYiAUXuoPZoYlYPQlXqwlYoAEc5EzMmUFRmclRnY1Ys8TPwXiVUUlYkgFUAICcAElXAgiQAISM0PWPgIVP3XTPxTSM8fCNJDyL3jiB..."
						}
,
						"snapshotlist" : 						{
							"current_snapshot" : 0,
							"entries" : [ 								{
									"filetype" : "C74Snapshot",
									"version" : 2,
									"minorversion" : 0,
									"name" : "Zebra 2.9.3",
									"origin" : "Zebra2.vstinfo",
									"type" : "VST",
									"subtype" : "Instrument",
									"embed" : 1,
									"snapshot" : 									{
										"pluginname" : "Zebra2.vstinfo",
										"plugindisplayname" : "Zebra 2.9.3",
										"pluginsavedname" : "",
										"pluginsaveduniqueid" : 0,
										"version" : 1,
										"isbank" : 0,
										"isbase64" : 1,
										"blob" : "26765.CMlaKA....fQPMDZ....ALUSDIC...P.....AjlaoQWZgwVZ5UF..........................fVTiDTS8nUYhIWXxnvHVUlby0iLvTCLvnvHE4FYoEla8vVZzQGakovHt0VOyPiBiz1b831atUlBiz1b8zzajcEZrovHsMWOPkFcig1UJLRay0yPzIGaAovHsMWOCQmbrIjBiz1b8vjYucTLJLRay0CSl81QxnvHsMWOGEFckovHsMWOKUVdF8FaJLRay0yRkkmQuwlLJLRay0iUkw1aikFc4ovHsMWOAQ0a0MFZJLRay0SPxAWSuQlBiz1b8Djbv0DYxnvHsMWOE4lcwnvHsMWOE4lcxnvHsMWOE4lcynvHsMWOE4lcznvHsMWOMMUQGEiBiz1b8zzTEcjLJLRay0SSSUzQynvHsMWOMMUQGQiBiz1b8vjYuEiBiz1b8vjYuIiBiz1b8vjYuMiBiz1b8vjYuQiBiz1b8zTSgAWLJLRay0SSMEFbxnvHsMWOM0TXvMiBiz1b8zTSgAGMJLRay0SSMkFdwnvHsMWOM0TZ3IiBiz1b8zTSog2LJLRay0SSMkFdznvHtYWO0nvHsYWOGEFckovHsYWOE4lcwnvHsYWOE4lcxnvHsYWOE4lcynvHsYWOE4lcznvHi0VOsEVZtovPi8Db8fCMt.CLJLBSF8zQ8DiBivjQOcjL8DiBiLVa8.0PuIWYJf0WwzCLt.CLJj0WwzCLt.CLJf0WxzCLt.CLJj0WxzCLt.CLJf0WyzCLt.CLJj0WyzCLt.CLJf0WzzCLt.CLJj0WzzCLt.CLJzDUwDSOOM0PwnCU04VYJzDSwDSOv3hLvnPSRESL8zBLtHCLJzDUwHSOOM0PwnCQzUmaJzDSwHSOsjiKv.iBMIULxzSKwDiKv.iBMQULyzySSMjL5PDc04lBMwTLyzyMt.CLJzjTwLSO33BLvnPSTECM831atUlNgM2boclakQlBMwTLzzSMv3BLvnPSRECM8zRMv3BLvnPSTESM831atUlNgM2boclakQlBMwTL0zCM33BLvnPSRESM8zBM33BLvnPSTEiM831atUlNgM2boclakQlBMwTL1zSMv3BLvnPSREiM8zRMv3BLvnPSTEyM831atUlNgM2boclakQlBMwTL2zSMv3BLvnPSREyM8zRMv3BLvnPSTECN831atUlNgM2boclakQlBMwTL3zSMv3BLvnPSRECN8zRMv3BLvnPSTISL8TjSVMiNSU2bJzDSxDSO1HiKv.iBMIkLwzSKyTiKv.iBMQkLxziau4VY5D1byk1YtUFYJzDSxHSO0.iKv.iBMIkLxzSK0.iKv.iBMQkLyziau4VY5D1byk1YtUFYJzDSxLSO0.iKv.iBMIkLyzSK0.iKv.iBMQkLzziau4VY5D1byk1YtUFYJzDSxPSO0.iKv.iBMIkLzzSK0.iKv.iBMQkL0ziau4VY5D1byk1YtUFYJzDSxTSO0.iKv.iBMIkL0zSK0.iKv.iBMQkL1ziau4VY5D1byk1YtUFYJzDSxXSO0.iKv.iBMIkL1zSK0.iKv.iBMQkL2ziau4VY5D1byk1YtUFYJzDSxbSO0.iKv.iBMIkL2zSK0.iKv.iBMQkL3ziau4VY5D1byk1YtUFYJzDSxfSO0.iKv.iBMIkL3zSK0.iKv.iBMQ0LwziUCYTL5XTSwnPSLMSL8PCNt.CLJzjTyDSOsbiLt.CLJzDUyHSOt8lakoSXyMWZm4VYjoPSLMiL8TCLt.CLJzjTyHSOsTCLt.CLJzDUyLSOt8lakoSXyMWZm4VYjoPSLMyL8TCLt.CLJzjTyLSOsTCLt.CLJzDUyPSOt8lakoSXyMWZm4VYjoPSLMCM8TCLt.CLJzjTyPSOsTCLt.CLJzDUyTSOt8lakoSXyMWZm4VYjoPSLMSM8PCNt.CLJzjTyTSOsPCNt.CLJzDUyXSOt8lakoSXyMWZm4VYjoPSLMiM8TCLt.CLJzjTyXSOsTCLt.CLJzDUybSOt8lakoSXyMWZm4VYjoPSLMyM8PCNt.CLJzjTybSOsPCNt.CLJzDUyfSOt8lakoSXyMWZm4VYjoPSLMCN8TCLt.CLJzjTyfSOsTCLt.CLJzDUzDSOVMjQwniTkMmBMwDMwzyL43BLvnPSRQSL8HCNt.CLJzDUzHSOVMTPwniUuwlLJzDSzHSOv3BLvnPSRQiL8LSMt.CLJzDUzLSOVMTPwniUuwVLJzDSzLSOv3BLvnPSRQyL8zRL23BLvnPSTQCM831atUlNgM2boclakQlBMwDMzzCM33BLvnPSRQCM8zBM33BLvnPSTQSM831atUlNgM2boclakQlBMwDM0zSMv3BLvnPSRQSM8zRMv3BLvnPSTQiM831atUlNgM2boclakQlBMwDM1zCM33BLvnPSRQiM8zBM33BLvnPSTQyM831atUlNgM2boclakQlBMwDM2zSMv3BLvnPSRQyM8zRMv3BLvnPSTQCN831atUlNgM2boclakQlBMwDM3zSMv3BLvnPSRQCN8zRMv3BLvnPSTUSL87zTCQiNV8FaJzDS0DSOw.CMt.CLJzjT0DSOw.CLt.CLJzDU0HSOOM0PznCQzUmaJzDS0HSOv3BLvnPSRUiL8zhLz3BLvnPSTUyL87zTCQiNSYDVwnPSLUyL8.iKv.iBMIUMyzCM43BLvnPSTUCM831atUlNgM2boclakQlBMwTMzzSMv3BLvnPSRUCM8zRMv3BLvnPSTUSM831atUlNgM2boclakQlBMwTM0zSMv3BLvnPSRUSM8zRMv3BLvnPSTUiM831atUlNgM2boclakQlBMwTM1zSMv3BLvnPSRUiM8zRMv3BLvnPSTUyM831atUlNgM2boclakQlBMwTM2zSMv3BLvnPSRUyM8zRMv3BLvnPSTUCN831atUlNgM2boclakQlBMwTM3zSMv3BLvnPSRUCN8zRMv3BLvnPSTYSL83zaoMWYwniUuwlBMwjMwzSLv.iKv.iBMIkMwziLv.iKv.iBMQkMxzySSMTL5X0aroPSLYiL8.iKv.iBMIkMxzSKw.CLt.CLJzDU1LSOOM0PxniUuwlBMwjMyzCLt.CLJzjT1LSOsDCLv3BLvnPSTYCM831atUlNgM2boclakQlBMwjMzzSMv3BLvnPSRYCM8zRMv3BLvnPSTYSM831atUlNgM2boclakQlBMwjM0zSMv3BLvnPSRYSM8zRMv3BLvnPSTYiM831atUlNgM2boclakQlBMwjM1zSMv3BLvnPSRYiM8zRMv3BLvnPSTYyM831atUlNgM2boclakQlBMwjM2zSMv3BLvnPSRYyM8zRMv3BLvnPSTYCN831atUlNgM2boclakQlBMwjM3zSMv3BLvnPSRYCN8zRMv3BLvnPSTcSL8TjSVIiNAQ2ZJzDS2DSOyLiKv.iBMI0MwzSKwLiKv.iBMQ0MxzSQNYkL5PTYioPSLciL8DSMt.CLJzjT2HSOsHCMt.CLJzDU2LSOE4jUwnyT0MmBMwzMyzSLt.CLJzjT2LSOsLSNt.CLJzDU2PSOE4jUwnCQkMlBMwzMzzCLt.CLJzjT2PSOsTCLt.CLJzDU2TSOE4jUwniTkwlBMwzM0ziL23BLvnPSRcSM8zRLv3BLvnPSTciM831atUlNgM2boclakQlBMwzM1zSMv3BLvnPSRciM8zRMv3BLvnPSTcyM831atUlNgM2boclakQlBMwzM2zSMv3BLvnPSRcyM8zRMv3BLvnPSTcCN831atUlNgM2boclakQlBMwzM3zSMv3BLvnPSRcCN8zRMv3BLvnPSTgSL8PTYrEVdwnSSogmBMwDNwzCLt.CLJzjT3DSOsTCNt.CLJzDU3HSONUmTkYWL5zTZ3oPSLgiL8fiLt.CLJzjT3HSO2TiKv.iBMQENyziVME1b5zTXyQmBMwDNyziL43BLvnPSRgyL8HyLt.CLJzDU3PSONUmTkYWL5LUZ5UlBMwDNzzCLt.CLJzjT3PSOsDSL23BMwnPSTgSM83TcRUlcwnCQkMVX4oPSLgSM8DiKv.iBMIEN0zSKwbiKv.iBMQEN1ziS0IUY1EiNT8lakoPSLgiM8.iKv.iBMIEN1zyLx3BLvnPSTgyM831atUlNgM2boclakQlBMwDN2zSMv3BLvnPSRgyM8zRMv3BLvnPSTgCN831atUlNgM2boclakQlBMwDN3zSMv3BLvnPSRgCN8zRMv3BLvnPSMQUL831atUlNgM2boclakQlBM0zTwzCLJzTSDESOv3BLvnPSMY0TwzCLJzTSVQTL8.iKv.iBM0DUxziau4VY5D1byk1YtUFYJzTSSISOvnPSMQjL8.iKv.iBM0jUSISOvnPSMYEQxzCLt.CLJzTSTMSOt8lakoSXyMWZm4VYjoPSMM0L8.iBM0DQyzCLt.CLJzTSVM0L8.iBM0jUDMSOv3BLvnPSMQEM831atUlNgM2boclakQlBM0zTzzCLJzTSDQSOv3BLvnPSMY0TzzCLJzTSVQDM8.iKv.iBM0DU0ziau4VY5D1byk1YtUFYJzTSSUSOvnPSMQTM8.iKv.iBM0jUSUSOvnPSMYEQ0zCLt.CLJzTSTYSOt8lakoSXyMWZm4VYjoPSMMkM8.iBM0DQ1zCLt.CLJzTSVMkM8.iBM0jUDYSOv3BLvnPSMQ0M831atUlNgM2boclakQlBM0zT2zCLJzTSDcSOv3BLvnPSMY0T2zCLJzTSVQzM8.iKv.iBM0DU3ziau4VY5D1byk1YtUFYJzTSSgSOvnPSMQDN8.iKv.iBM0jUSgSOvnPSMYEQ3zCLt.CLJzTSTkSOt8lakoSXyMWZm4VYjoPSMMUN8.iBM0DQ4zCLt.CLJzTSVMUN8.iBM0jUDkSOv3BLvnPSMQULvziau4VY5D1byk1YtUFYJzTSSECL8.iBM0DQw.SOv3BLvnPSMY0Tw.SOvnPSMYEQw.SOv3BLvnPSMQULwziau4VY5D1byk1YtUFYJzTSSESL8.iBM0DQwDSOv3BLvnPSMY0TwDSOvnPSMYEQwDSOv3BLvnPSMQULxziau4VY5D1byk1YtUFYJzTSSEiL8.iBM0DQwHSOv3BLvnPSMY0TwHSOvnPSMYEQwHSOv3BLvnvTBE1bk0iLJL0co41Y8.iKv.iBSQkbocVOwn.TPI2ap0CLJ.kQuwFY8.iBPYTZrUVOwnvQFkFak0iLJbzTiEFak0CLJLDZLEVd8.iBSUmbx8TOvnfTkYWOwHCL4HiBLUDQ8.iKv.iBPEzQE0CLJ.UXmU1bO4VOvnvPuIWYN0yLJLEaoMVY8PiBUkzWuAWOznPSoQVZA0SMJzTZjkFT8XiBDYzarQVO2nvPzIGaA0iLJLDcxwlP8DSLJLxXs0CSF8zQJLUdtMVOsHiBTIWZm0CLJbUX1UVOwn.TnMWY8.iKv.iBREFck0CNv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWO3nPUWYWOvnvHi0VOLYzSGIiBSkmai0SKxn.Uxk1Y8.iBWElck0SLJ.EZyUVOv3BLvnfTgQWY8DCLv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWO4nPUWYWOvnvHi0VOVMzPJLBSF8TL8DiBivjQOISOwnvHLYzSyzSLJLBSF8DM8DiBV8VZiU1b8DiBV8VZiklam0CLJzzajUVOvn.TuIGcg0CLt.CLJ.kP8HiBPIDQ8HiBAI2Ti0iLJDjbOIGY8.iBAIGSv0CLJDjbOMFc8.iBAIGSL0SL1nPPxQkb8.iBDImYz0SLJzDU041T8.iBMQUct4TOw.iBMQUctQUOwDiBTI2bv0SKwHiBFQUct0CLt.CLJ.0axQmTm0SLv.iKv.iBP8lbzEVS8.iBP8lbzElL8zRLv.iKv.iBAcFckESOxnPPzIGbwzCLJDjcuMVL8DiBA0VcrESOwnPPs8FYwzCLJDTSDAGcwzCLt.CLJDTSDAmPwzCLt.CLJDzYzUlL8HiBAQmbvISOvnPP181XxzSLJDTa0wlL8DiBA01ajISOvnPPMQDbzISOv3BLvnPPMQDbBISOv3BLvnPPmQWYyziLJDDcxA2L8.iBAY2aiMSOwnPPsUGayzSLJDTauQ1L8.iBA0DQvQ2L8.iKv.iBA0DQvIzL8.iKv.iBAcFckQSOxnPPzIGbzzCLJDjcuMFM8DiBA0VcrQSOwnPPs8FYzzCLJDTSDAGczzCLt.CLJDTSDAmPzzCLt.CLJDzYzUVM8HiBAQmbvUSOvnPP181X0zSLJDTa0wVM8DiBA01ajUSOvnPPMQDbzUSOv3BLvnPPMQDbBUSOv3BLvnPPmQWY1ziLJDDcxAmM8.iBAY2aiYSOwnPPsUGa1zSLJDTauQlM8.iBA0DQvQmM8.iKv.iBA0DQvIjM8.iKv.iBAcFckcSOxnPPzIGb2zCLJDjcuM1M8DiBA0VcrcSOwnPPs8FY2zCLJDTSDAGc2zCLt.CLJDTSDAmP2zCLt.CLJDzYzUFN8HiBAQmbvgSOvnPP181X3zSLJDTa0wFN8DiBA01ajgSOvnPPMQDbzgSOv3BLvnPPMQDbBgSOv3BLvnPPmQWY4ziLJDDcxAWN8.iBAY2aikSOwnPPsUGa4zSLJDTauQVN8.iBA0DQvQWN8.iKv.iBA0DQvITN8.iKv.iBAcFckECL8HiBAQmbvECL8.iBAY2aiECL8DiBA0VcrECL8DiBA01ajECL8.iBA0DQvQWLvzCLt.CLJDTSDAmPw.SOv3BLvnPPmQWYwDSOxnPPzIGbwDSOvnPP181XwDSOwnPPsUGawDSOwnPPs8FYwDSOvnPPMQDbzESL8.iKv.iBA0DQvITLwzCLt.CLJDzYzUVLxziLJDDcxAWLxzCLJDjcuMVLxzSLJDTa0wVLxzSLJDTauQVLxzCLJDTSDAGcwHSOv3BLvnPPMQDbBEiL8.iKv.iBAcFckEyL8HiBAQmbvEyL8.iBAY2aiEyL8DiBA0VcrEyL8DiBA01ajEyL8.iBA0DQvQWLyzCLt.CLJDTSDAmPwLSOv3BLvnPPmQWYwPSOxnPPzIGbwPSOvnPP181XwPSOwnPPsUGawPSOwnPPs8FYwPSOvnPPMQDbzECM8.iKv.iBA0DQvITLzzCLt.CLJDzYzUVL0ziLJDDcxAWL0zCLJDjcuMVL0zSLJDTa0wVL0zSLJDTauQVL0zCLJDTSDAGcwTSOv3BLvnPPMQDbBESM8.iKv.iBAcFckEiM8HiBAQmbvEiM8.iBAY2aiEiM8DiBA0VcrEiM8DiBA01ajEiM8.iBA0DQvQWL1zCLt.CLJDTSDAmPwXSOv3BLvnvHi0VOE4jUwnPSuQVY8.iBo0zajUVOvnvbM8FYk0CLJjlaoQWOv3BLvnPPzsVOv3BLvn.QkMVOw.CLt.CLJLUcy0CMv3BLvnvT0MGU8.iKv.iBSU2bxzCLt.CLJHUYr0yLv3BLvnfUkwVOv3BLvnfUxjTOv3BLvnfUxDTOv3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVIiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8HCLt.CLJPTYi0iMv3BLvnvT0MWOv3BLvnvT0MGU8.iKv.iBSU2bxzCLt.CLJHUYr0SMv3BLvnfUkwVO4.iKv.iBVISR8.iKv.iBVISP8zhLz3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVMiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8.iKv.iBDU1X8.iKv.iBSU2b8PSNtTCLJLUcyQUOv3BLvnvT0MmL8.iKv.iBRUFa8DCLv3BLvnfUkwVOv3BLvnfUxjTOv3BLvnfUxDTOv3BLvnfUxPTOv3BLvnfUxLUOv3BLvnfUxXjT8.iKv.iBVIyTxzCLt.CLJXkLR0CLt.CLJrjLI0CLt.CLJrjLA0CLt.CLJrjLD0CLt.CLJrjLS0CLt.CLJrjLFIUOv3BLvnvRxLkL8.iKv.iBKIiT8.iKv.iBSw1avUVOsXCLt.CLJPkPgMWY8.iBiLVa8TjSVQiBM8FYk0CLJjVSuQVY8.iBy0zajUVOvnPZtkFc8.iKv.iBAQ2Z8.iKv.iBDU1X8TCLt.CLJLUcy0SLv.iKv.iBSU2bT0CLt.CLJLUcyISOv3BLvnfTkwVOw.iKv.iBVUFa8.iKv.iBVISR8.iKv.iBVISP8.iKv.iBVICQ8.iKv.iBVIyT8.iKv.iBVIiQR0CLt.CLJXkLSISOv3BLvnfUxHUOv3BLvnvRxjTOv3BLvnvRxDTOv3BLvnvRxPTOv3BLvnvRxLUOv3BLvnvRxXjT8.iKv.iBKIyTxzCLt.CLJrjLR0CLt.CLJLEauAWY8zhMv3BLvn.UBE1bk0CLJLxXs0SSSUzQwn.UsUka8DiBE4lc8DiLJXUYr0CLt.CLJDDcq0CLt.CLJvDbz0CLt.CLJHUYr0CLt.CLJPkbocVOvnvHi0VOMMUQGIiBT0VUt0SLJTja10SLynfUkwVOv3BLvnPPzsVOv3BLvn.SvQWOv3BLvnfTkwVOv3BLvn.Uxk1Y8.iBiLVa8zzTEczLJPUaU4VOwnPQtYWOwPiBVUFa8.iKv.iBAQ2Z8.iKv.iBLAGc8.iKv.iBRUFa8.iKv.iBTIWZm0CLJLxXs0SSSUzQzn.UsUka8DiBE4lc8DSMJXUYr0CLt.CLJDDcq0CLt.CLJvDbz0CLt.CLJHUYr0CLt.CLJPkbocVOvnvHi0VOLYzSwnvT441X8zxLJPkbocVOwnvUgYWY8.iBPg1bk0CLt.CLJHUXzUVO3.iKv.iBA0Fb8DCLv3BLvnvTrU1c8DiBNMGcv0SL1nvTzA2b8DiMJT0U10CLJPDa40CLt.CLJPTSSESOwn.QMQTL8DCLv3BLvnfQMMUL8.iBF0DQwzCLt.CLJLxXs0CSF8jLJLUdtMVOsHiBTIWZm0SLJbUX1UVOwn.TnMWY8.iKv.iBREFck0SLv.iKv.iBA0Fb8DCLv3BLvnvTrU1c8DiBNMGcv0SL1nvTzA2b8DyMJT0U10CLJPDa40CLt.CLJPTSSESOvn.QMQTL8.iKv.iBF0zTwzCLJXTSDESOv3BLvnvHi0VOLYzSynvT441X8zhLJPkbocVOwnvUgYWY8DiBPg1bk0CLt.CLJHUXzUVOw.CLt.CLJDTav0SLv.iKv.iBSwVY20SLJ3zbzAWOwXiBSQGby0SL3nPUWYWOvn.QrkWOv3BLvn.QMMUL8.iBD0DQwzCLt.CLJXTSSESOvnfQMQTL8.iKv.iBiLVa8vjQOQiBSkmai0SKxn.Uxk1Y8DiBWElck0SLJ.EZyUVOv3BLvnfTgQWY8DCLv3BLvnPPsAWOw.CLt.CLJLEakcWOwnfSyQGb8DiMJLEcvMWOwjiBUckc8.iBDwVd8.iKv.iBD0zTwzCLJPTSDESOv3BLvnfQMMUL8.iBF0DQwzCLt.CLJLxXs0SSMEFbwnPSuQVY8HiBMMkbi0SL1nvTzA2b8HCLJ3Tcs0yLJLxXs0SSMEFbxnPSuQVY8.iBMMkbi0CLJLEcvMWOxDiBNUWa8DyMJLxXs0SSMEFbynPSuQVY8LiBMMkbi0CLJLEcvMWOxHiBNUWa8DyMJLxXs0SSMEFbznPSuQVY8LiBMMkbi0CLJLEcvMWOxLiBNUWa8DyMJLxXs0SSMkFdwn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdxn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdyn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0SSMkFdzn.U4AWY8.iBM8FYwzCLJzzajISOvnPSuQ1L8.iBCMGc8TCLt.CLJLxXs0yQxkFYJbjboQVOxPiBGITdv0CLJLxXs0ySSMTLJbUX1UVOvn.U04VY8.iKv.iBKUVdSMFa8DCLv3BLvn.UMMkbi0CLJPUSDAGc8.iKv.iBPg1bk0SMv3BLvn.TnMWSSI2X8.iBPg1bMQDbz0CLt.CLJbkS00VO03BLvnvUPMkbi0CLJbETDAGc8.iKv.iBVQ2aD0iM03BLvnvP0Imck0iL0n.TxU1X8TiKv.iBFgULTAWOvnvTFgUL8.iKv.iBFgULSMVOvnfQXECQz0CLt.CLJXDVxPEb8.iBSYDVxzCLt.CLJXDVxL0X8.iBFgkLDQWOv3BLvn.TuwVd8DiBDQWct0SK33BLvnvRVM2X8HiMJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SMv3BLvn.T201St0CLJbUXTIVOxbiBRUFTnMWOvnfSuIWa8DSMt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0iL3nvHi0VOOM0PxnvUgYWY8.iBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOxXiBT0DQvQWOwHiKv.iBPg1bk0SMv3BLvn.TnMWSSI2X8TiBPg1bMQDbz0iLx3BLvnvUNUWa8TiKv.iBWA0TxMVOvnvUPQDbz0CLt.CLJXEcuQTO0TiKv.iBCUmb1UVOxjiBPIWYi0SMt.CLJXDVwPEb8.iBSYDVwzCLt.CLJXDVwL0X8.iBFgULDQWOv3BLvnfQXICUv0CLJLkQXISOv3BLvnfQXIyTi0CLJXDVxPDc8.iKv.iBP8Fa40SLJPDc04VOz3BLvnvRVM2X8LCLJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SLv.iKv.iBPcWaO4VOwnvUgQkX8LSLJHUYPg1b8.iBN8lbs0SL03BLvnfTk4FY8.iBF0Fc5UVOvnvUT4TXsUVOyHiBiLVa87zTCMiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvn.TnMWY8TCLt.CLJ.EZy0zTxMVOvn.TnMWSDAGc8.iKv.iBW4Tcs0SMt.CLJbETSI2X8.iBWAEQvQWOv3BLvnfUz8FQ8.iKv.iBCUmb1UVOyLiBPIWYi0SMt.CLJXDVwPEb8.iBSYDVwzCLt.CLJXDVwL0X8.iBFgULDQWOv3BLvnfQXICUv0CLJLkQXISOv3BLvnfQXIyTi0CLJXDVxPDc8.iKv.iBP8Fa40CLJPDc04VOv3BLvnvRVM2X8LCMJX0ar0SLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvnvT441X8.iKv.iBS41XSMVOvnvTtMFQz0CLt.CLJLkai8ja8.iBP8FaW0SMv3BLvn.T201St0CLJbUXTIVOyTiBRUFTnMWOvnfSuIWa8DSMt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0yL1nvHi0VOOM0PznvUgYWY8LiBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOvn.UMQDbz0CLt.CLJ.EZyUVOv3BLvn.TnMWSSI2X8.iBPg1bMQDbz0CLt.CLJbkS00VOw3BLvnvUPMkbi0CLJbETDAGc8DSMt.CLJXEcuQTO1TiKv.iBCUmb1UVOybiBPIWYi0yMt.CLJXDVwPEb8DiMJLkQXESOv3BLvnfQXEyTi0CLJXDVwPDc8.iKv.iBFgkLTAWOvnvTFgkL8.iKv.iBFgkLSMVOvnfQXICQz0CLt.CLJ.0arkWOxn.QzUma8HiKv.iBKY0bi0yL3nfUuwVOv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJLUdtMVOv3BLvnvTtM1Ti0CLJLkaiQDc8.iKv.iBS41XO4VOvn.Tuw1U8DCLv3BLvn.T201St0CLJbUXTIVOyjiBRUFTnMWOvnfSuIWa8HCLt.CLJHUYtQVOvnfQsQmdk0CLJbEUNEVak0CMvnvHi0VON8VZyUVLJPUdvUVOvnfQwzSMv3BLvnfQwLkbi0SL0nfQwPDbz0SLv.iKv.iBFISOv3BLvnfQxLkbi0CLJXjLDAGc8.iKv.iBKY0bi0CMwnfUuwVOv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvHi0VON8VZyUlLJPUdvUVOvnfQwzSLv.iKv.iBFEyTxMVOvnfQwPDbz0CLt.CLJXjL8.iKv.iBFIyTxMVOvnfQxPDbz0CLt.CLJrjUyMVOzHiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvHi0VOVMjQwn.U4AWOvnvP0QWOybiKv.iBRU1b8TiKv.iBDImc8.iKv.iBGEVZt0CLt.CLJXTSwzSLv.iKv.iBFMUL8DSMJXTSxziMv3BLvnfQSISOwDiBKUVdSMFa8PiLt.CLJLxXs0iUCYjLJPUdv0SL2nvP0QWO2fiKv.iBRU1b8LSMt.CLJPjb10iLv3BLvnvQgkla8.iKv.iBF0TL8LCNt.CLJXzTwzSMJXTSxzCLt.CLJXzTxzCLJrTY4M0Xr0CLt.CLJLxXs0iUCYzLJPUdv0SL3nvP0QWOwTCLt.CLJHUYy0CLt.CLJPjb10CLt.CLJbTXo4VOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnvHi0VOVMjQzn.U4AWOwfiBCUGc8DSMv3BLvnfTkMWOv3BLvn.QxYWOv3BLvnvQgkla8.iKv.iBF0TL8.iKv.iBFMUL8.iBF0jL8.iKv.iBFMkL8.iBKUVdSMFa8.iKv.iBiLVa8XTSOEiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzLiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOIiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzPiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOMiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzTiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8XTSOQiBWElck0CLJPUctUVOv3BLvnvRkk2TiwVOw.CLt.CLJPUSSI2X8.iBT0DQvQWOv3BLvnfQM0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnfUz8FQ8.iKv.iBDQWct0CLt.CLJrjUyMVOzXiBV8Fa8DCLv3BLvnfUuw1Ti0CLJX0arQDc8.iKv.iBPEla8.iKv.iBPElaSMVOvn.Tg4FQz0CLt.CLJ.0arkWOvn.Tuw1U8DCLv3BLvnvQkQmb8.iBiLVa8LzasIVLJzzajUVOvn.U04VY8.iKv.iBKUVdSMFa8DCLv3BLvn.UMMkbi0CLJPUSDAGc8.iKv.iBDUFct0CLt.CLJXEcuQTOv3BLvnfQB0CLt.CLJXjPSI2X8.iBFIDQvQWOv3BLvn.Qg0Fb8.iKv.iBD0FbSI2X8.iBD0FbDAGc8.iKv.iBEg2X8.iKv.iBI4lZ8DCLv3BLvnPRto1TxMVOvnPRtoFQvQWOv3BLvn.UtUVO0.iKv.iBT4VYSI2X8.iBT4VYDAGc8.iKv.iBSU1X8.iKv.iBSU1XSI2X8.iBSU1XDAGc8.iKv.iBDk1bz0CLt.CLJPjb40CLt.CLJX0ar0iLv.iKv.iBV8FaSMVOvnfUuwFQz0CLt.CLJ.UXt0CLt.CLJ.UXtM0X8.iBPElaDQWOv3BLvn.TuwVd8.iBP8FaW0SLv.iKv.iBFkFar0CLJLxXs0yPu0lXxnPSuQVY8.iBTUmak0CLt.CLJrTY4M0Xr0SLv.iKv.iBT0zTxMVOvn.UMQDbz0CLt.CLJPTYz4VOv3BLvnfUz8FQ8.iKv.iBFITOv3BLvnfQBMkbi0CLJXjPDAGc8.iKv.iBDEVav0CLt.CLJPTavMkbi0CLJPTavQDbz0CLt.CLJTDdi0CLt.CLJjjap0SLv.iKv.iBI4lZSI2X8.iBI4lZDAGc8.iKv.iBT4VY8TCLt.CLJPkakMkbi0CLJPkakQDbz0CLt.CLJLUYi0CLt.CLJLUYiMkbi0CLJLUYiQDbz0CLt.CLJPTZyQWOv3BLvn.QxkWOv3BLvnfUuwVOx.CLt.CLJX0arM0X8.iBV8FaDQWOv3BLvn.Tg4VOv3BLvn.Tg41Ti0CLJ.UXtQDc8.iKv.iBP8Fa40CLJ.0arcUOw.CLt.CLJXTZrwVOvnvHi0VOSgVXvUVLJPUdvUVOyn.QkAGcn0CLt.CLJPTSSI2X8.iBD0DQvQWOv3BLvnPQjcVY8DCLv3BLvnPQMMkbi0CLJTTSDAGc8.iKv.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.Ro8Tcz0CLt.CLJLxXs0yTnEFbkIiBTkGbk0yLJPTYvQGZ8.iKv.iBD0zTxMVOvn.QMQDbz0CLt.CLJTDYmUVOw.CLt.CLJTTSSI2X8.iBE0DQvQWOv3BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJfTZOUGc8.iKv.iBiLVa8zTZ3EiBPEla8.iKv.iBMkFd8TCLt.CLJ.kaMQVOvn.Tt0DQ8.iKv.iBP4VSS0CLJLxXs0SSogmLJ.UXt0CLt.CLJzTZ30SMv3BLvn.Tt0DY8.iBP4VSD0CLt.CLJ.kaMMUOvnvHi0VOMkFdyn.Tg4VOv3BLvnPSogWO0.iKv.iBP4VSj0CLJ.kaMQTOv3BLvn.Tt0zT8.iBiLVa8zTZ3QiBPEla8.iKv.iBMkFd8TCLt.CLJ.kaMQVOvn.Tt0DQ8.iKv.iBP4VSS0CLJLxXs0CVMYTLJPUdv0CLJLTcz0SL0.iKv.iBRU1b8.iKv.iBF0TL8.iKv.iBFMUL8.iBF0jL8.iKv.iBFMkL8.iBKUVdSMFa8.iKv.iBF8jYl0CLt.CLJXzSM8FY8.iKv.iBF8zTxMVOvn.VF0TOv3BLvn.VF0DQ8.iKv.iBXYTSS0CLJHTZgMWOv3BLvnvSL8VXj0CLt.CLJLDaoM1Z8.iKv.iBDImc8DiBR8Vcz0CLJPUdvISOsDiBiLVa8fUSFIiBTkGb8.iBCUGc8DSMv3BLvnfTkMWOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnfQOYlY8.iKv.iBF8TSuQVOv3BLvnfQOMkbi0CLJfkQM0CLt.CLJfkQMQTOv3BLvn.VF0zT8.iBBkVXy0CLt.CLJ7DSuEFY8.iKv.iBCwVZisVOv3BLvn.QxYWOwnfTuUGc8.iBTkGbxzSKwnvHi0VOSITLJHUXtcVY8.iBFIWYw0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnvSlY1b8.iKv.iBO0zTxMVOvnvSMQDbz0CLt.CLJzTZ30SMv3BLvnPSMMkbi0CLJzTSDAGc8.iKv.iBiLVa8LkPxnfTg41Yk0CLJXjbkEWOv3BLvnfQMMkbi0CLJXTSDAGc8.iKv.iBOYlYy0CLt.CLJ7TSSI2X8.iBO0DQvQWOv3BLvnPSogWO0.iKv.iBM0zTxMVOvnPSMQDbz0CLt.CLJLxXs0CQoMGcwn.U4AWY8.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.TxUFUowFc8.iKv.iBPMGcTkFaz0CLt.CLJLjazYjbkEWOw.CLt.CLJvza20CLt.CLJfTZmgVOv3BLvn.TuMGcFwFc8.iBiLVa8PTZyQmLJPUdvUVOvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJ.kbkQUZrQWOv3BLvn.TyQGUowFc8.iKv.iBC4FcFIWYw0SLv.iKv.iBL81c8.iKv.iBHk1Yn0CLt.CLJ.0ayQmQrQWOvnvHi0VOF8FajEiBRkFbvwVYy0CMJXzarQ1b8DCLt.CLJXTSuQVOv3BLvnfQSI2X8.iBBkVXy0CLt.CLJHTSuQVOv3BLvnfPSI2X8.iBFIUXzk1a8TCLt.CLJXzTr8Fbk0SLv3BLvnvPA41Yr0SMv3BLvn.SoYWY8.iBiLVa8XzarQlLJHUZvAGakMWOznfQuwFYy0SLv3BLvnfQM8FY8.iKv.iBFMkbi0CLJHTZgMWOv3BLvnfPM8FY8.iKv.iBBMkbi0CLJXjTgQWZu0SMv3BLvnfQSw1avUVOw.iKv.iBCEjamwVO0.iKv.iBLklck0CLJLxXs0iUCETLJ.UXtESOv3BLvn.Tg4VSSESOvn.Tg4VSDESOv3BLvnfUuwVL8TCLt.CLJX0PAESOwnPSuQ1TxMVL8.iBM8FYDAGcwzCLt.CLJ.UXtISOv3BLvn.Tg4VSSISOvn.Tg4VSDISOv3BLvnfUuwlL8.iKv.iBVMTPxzSLJzzajMkbiISOvnPSuQFQvQmL8.iKv.iBPElayzCLt.CLJ.UXt0zTyzCLJ.UXt0DQyzCLt.CLJX0arMSOxTiKv.iBVMTPyzSLJzzajMkbiMSOvnPSuQFQvQ2L8.iKv.iBPElazzCLt.CLJ.UXt0zTzzCLJ.UXt0DQzzCLt.CLJX0arQSOv3BLvnfUCEDM8DiBM8FYSI2XzzCLJzzajQDbzQSOv3BLvnPSTESOvnPSTISOvnPSTMSOvnPSTQSOvn.TBESOvn.TBISOvn.TBMSOvn.TBQSOvnfP0MWL8.iBBU2bxzCLJHTcyMSOvnfP0MGM8.iBSUlajESOv3BLvnvTtMkbiESOvnvTtQDbzESOv3BLvnvTk4FYxzCLt.CLJLkaSI2XxzCLJLkaDAGcxzCLt.CLJDDczMUOwnvHi0VOGIWZjYDVJbjboQVOzbiBGITdv0CLJLxXs0SSuQlQXEiBM8FYk0CLJLTYtQWOwbiK0.iBSAWYj0SMv3BLvn.Tn8jYl0SMv3BLvn.QvQGZ8jiLtTCLJXTYkITOv3BLvnPSogWOxTiKv.iBLMTcz0CLt.CLJfzP0QWOw.CLt.CLJDUcgQVO1.iKv.iBQAGZy0iL03BLvn.SkEWOv3BLvn.RkEWOv3BLvnPTwzCLt.CLJDkL8.iKv.iBEE0at0CLJLxXs0SSuQlQXIiBM8FYk0iLJLTYtQWOz.iKv.iBSAWYj0SMt.CLJ.EZOYlY8TCLt.CLJPDbzgVOz.iKv.iBFUVYB0SMv3BLvnPSogWO0.iKv.iBLMTcz0CLt.CLJfzP0QWOw.CLt.CLJDUcgQVOv3BLvnPTvg1b8HSMt.CLJvTYw0CLt.CLJfTYw0CLt.CLJDUL8.iKv.iBQISOv3BLvnPQQ8la8DiBiLVa8PTYrEVdwnPSuQVY8HiBMkFd8LyLt.CLJXjP8.iKv.iBCITO1TiKv.iBLAUO2.iKv.iBHAUO0.iKv.iBDImc8.iKv.iBSkmaiESOznvT441XxzSNJLUdtM1L8PiBSkmaiQSOzn.UvzSLv.iKv.iBTESOw.CLt.CLJPkL8DCLv3BLvn.UyzSLv.iKv.iBPElawzSKw.CLt.CLJ.UXtISOw.CLt.CLJ.UXtMSOsDCLv3BLvn.Tg4FM8DCLv3BLvnvHi0VODUFagkmLJzzajUVOvnPSogWO0.iKv.iBFITOz.iKv.iBCITOv3BLvn.SP0yMv3BLvn.RP0yLv3BLvn.QxYWOv3BLvnvT441XwzSKwnvT441XxzSKwnvT441XyzSKwnvT441XzzSKwn.UvzCM03BLvn.UwzyLv3BLvn.UxzSLv.iKv.iBTMSOw.CLt.CLJ.UXtESOsDCLv3BLvn.Tg4lL8DCLv3BLvn.Tg41L8zRLv.iKv.iBPElazzSLv.iKv.iBiLVa8LEZgAWYyn.U4AWY8LiBDUFbzgVOv3BLvn.QMMkbi0CLJPTSDAGc8.iKv.iBEQ1Yk0SLv.iKv.iBE0zTxMVOvnPQMQDbz0CLt.CLJjjavUGc8.iKv.iBOUGcvUGc8.iKv.iBHk1S0QWOv3BLvnvHi0VOSgVXvUFMJPUdvUVOyn.QkAGcn0CLt.CLJPTSSI2X8.iBD0DQvQWOv3BLvnPQjcVY8DCLv3BLvnPQMMkbi0CLJTTSDAGc8.iKv.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.Ro8Tcz0CLt.CLJLxXs0SSogWMJ.UXt0CLt.CLJzTZ30SMv3BLvn.Tt0DY8.iBP4VSD0CLt.CLJ.kaMMUOvnvHi0VOMkFd1n.Tg4VOv3BLvnPSogWO0.iKv.iBP4VSj0CLJ.kaMQTOv3BLvn.Tt0zT8.iBiLVa8HUY1EiBM8FYk0CLJPjb40SLv.iKv.iBWUFc8TCLt.CLJXjP8bCLtTCLJPTXsAWOxLiKv.iBSkldk0yM03BLvnvTvQVO0.iKv.iBDAGc8TSMt.CLJPjQB0yMv3BLvn.QSkldk0yMy3BLvnPQMkFd8DCLv3BLvn.QM8FY8TSMt.CLJPzTvQVO0TiKv.iBPIWY8HCLt.CLJLxXs0yPu0Fbwn.U4AWY8DiBREFc8PCLt.CLJPEZxU1b8.iKv.iBAQGc8HCLt.CLJHUYr0iL03BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJLxXs0yPu0Fbxn.U4AWY8DiBREFc8PCLt.CLJPEZxU1b8.iKv.iBAQGc8HCLt.CLJHUYr0iL03BLvnPRtAWcz0CLt.CLJ7TczAWcz0CLt.CLJLxXs0SQQEiBlMVL8HCLt.CLJHWYyESOxTiKv.iBmEVZtESOv3BLvnfYiISOz.iKv.iBxU1bxziL03BLvnvYgklaxzCLt.CLJX1XyziMv3BLvnfbkM2L8HSMt.CLJbVXo41L8.iKv.iBlMFM8fCLt.CLJHWYyQSOxTiKv.iBmEVZtQSOv3BLvnvHi0VOEEkLJX1XwziLv3BLvnfbkMWL8HSMt.CLJbVXo4VL8.iKv.iBlMlL8PCLt.CLJHWYyISOxTiKv.iBmEVZtISOv3BLvnfYiMSO1.iKv.iBxU1byziL03BLvnvYgklayzCLt.CLJX1XzzCNv3BLvnfbkMGM8HSMt.CLJbVXo4FM8.iKv.iBiLVa8X0PFUiBTkGb8DCNJLTcz0SL0.iKv.iBRU1b8.iKv.iBDImc8.iKv.iBGEVZt0CLt.CLJXTSwzCLt.CLJXzTwzCLJXTSxzCLt.CLJXzTxzCLJrTY4M0Xr0CLt.CLJLxXs0iUCYjMJPUdv0SL3nvP0QWOwTCLt.CLJHUYy0CLt.CLJPjb10CLt.CLJbTXo4VOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnvHi0VOSIzLJHUXtcVY8.iBFIWYw0CLt.CLJXTSSI2X8.iBF0DQvQWOv3BLvnvSlY1b8.iKv.iBO0zTxMVOvnvSMQDbz0CLt.CLJzTZ30SMv3BLvnPSMMkbi0CLJzTSDAGc8.iKv.iBiLVa8fUSFMiBTkGb8.iBCUGc8DSMv3BLvnfTkMWOv3BLvnfQMESOv3BLvnfQSESOvnfQMISOv3BLvnfQSISOvnvRkk2TiwVOv3BLvnfQOYlY8.iKv.iBF8TSuQVOv3BLvnfQOMkbi0CLJfkQM0CLt.CLJfkQMQTOv3BLvn.VF0zT8.iBBkVXy0CLt.CLJ7DSuEFY8.iKv.iBCwVZisVOv3BLvn.QxYWOwnfTuUGc8.iBTkGbxzSKwnvHi0VODk1bzMiBTkGbk0CLJjjavUGc8.iKv.iBOUGcvUGc8.iKv.iBPIWYTkFaz0CLt.CLJ.0bzQUZrQWOv3BLvnvPtQmQxUVb8DCLv3BLvn.SucWOv3BLvn.RocFZ8.iKv.iBP81bzYDaz0CLJLxXs0CQoMGczn.U4AWY8.iBI4Fb0QWOv3BLvnvS0QGb0QWOv3BLvn.TxUFUowFc8.iKv.iBPMGcTkFaz0CLt.CLJLjazYjbkEWOw.CLt.CLJvza20CLt.CLJfTZmgVOv3BLvn.TuMGcFwFc8.iBiLVa83TcRUlcwn.TxUVOv3BLvn.Qg0Fb8fSLtTCLJPTYiEVd8byLtTCLJLUZ5UVOwLiLt.CLJP0atUVOsLiMt.CLJbUZjQGZ8XSNtTCLJzTZ30CLt.CLJLxXs0iVME1bJHUYzESOv3BLvnfTkQmL8.iKv.iBME1bz0yMv3BLvn.VYECS8PCNJfUVxvTOzjiBXk0LL0SMvn.VYQCS8TSLJfUVwPUO0HiBXkkLT0SMyn.VYMCU8TCMJfUVzPUO0TiBOM0PwzSM1nvSSMjL8TyMJ7zTCMSO0fiBOM0PzzSM4nPSSUzQwziMvnPSSUzQxziMwnPSSUzQyziMxnPSSUzQzziMynPSSUzQ0ziMznPSSUzQ1ziM0nPSSUzQ2ziM1nPSSUzQ3ziM2nfTkYWL8XCNJ.kayzCLJ.kazzCLJ.ka0zCLJ.ka1zCLJ.ka2zCLJ.ka3zCLJ.ka4zCLJ.kaw.SOvn.TtESL8.iBRE1XqASO1jiBRE1XqESO2.iBiLVa8vTX40TYso.SgkWSk0VL8.iBLEVdMUVaxzCLJvTX40TYsMSOvn.SgkWSk0FM8.iBLEVdMUVa0zCLJnfBJnvKu.xTkMFco8lafX1axARcmwVdfL1asAmbkM2bkQFHhklagIWdfPTXzElBu7BHD8jSmPEHT8TUCgDHTgTRSofBjPBIjbyMyPCLJ7SXgEVXoEFYvoiZqolZpoFauoSXgEVXoEFavoyYjc1Xk4VYhoCbtAGbnAGYvoCbsAGbnAGYvoyZqs1ZqsFauoiBq01Zqs1Zj8lNgEVXgEVXkElNiEVXgEVXgElNgElNjAmNrAmNj8lNkIlNkMlNr8lNmYlNkElNmIlNnAmNmAmNnovZ5LVX5fVY5vla5Pla5.Gb5D1Z5P1X5XFY5blZ5HVX5jVX5PVX5TlagPWPgIVPw.SYswzYjozYsMFbjUlSlElUJnzYuwjUmUFZjEjLyfCcIETLvX0YuY0SVozYsYUSHM1amkFUnEVPlEFSnQlUOYESm81XucVZTgVXAMiMzkTP2PiBzkTPw.SXjUVaUkVZp0VXkolYh8lavklZp81XroVaVwlZiYlag0VXVEVYwrFZpcVXv0VZgUFZhIFYu01Yo4Vauo.brMlTl0VSgglXo4VYoMlXlMVan8VYnsFamQ1YroVXoUFag4VaoklYgw1Zuc1apM1XqslXq4VYpAGagA2XjgFcJDVYAECLpE1ZsYlaSQWPgIVP4TVZLgFZJg1XmUlSUcFYJg1XwDjLzLCcXETLyfCcAElXAIiM0PGVAYSbAEiL2PmBXEjMwETLxbCcIETLvbVYHc1YJglYm01SNgFYmQlRm0FRAIiLwkWPwjTLAECVIETLqEVRAESagkTPw7VXIQWPgovXAUSbAMSb4ETLIESPwfURAEyZgkTPwzVXIETLuEVRAISQAEyUEETLNUTPwjUQAESREETLlEVQAEyYgUTPwfVXJTTPwfUQAESZoUTPwnVXEETLpkVQAEyZgUTPwrVZEETLrEVQAECaoUTPwzVXEETLskVQAEiagUTPw3VZEETLuElBEETLukVQAECbgUTPw.WZEEjLFETLgUlQAESXoYTPwDVaFETLWYTPwHVYFETLhklQAEiXsYTPw3jQAEyXkYTPwnvXoYTPwLVaFETLYYTPwPVYFETLjklQAECYsYTPwjjQAESYkYTPwTVZFETLk0lQAEiYgYTPwXVYFETLlklQAEiYJzlQAEyYgYTPwbVYFETLmklQAEyYsYTPwfVXFETLOYTPwfVZFETLn0lQAECVFETLoMlQAESZkYTPwj1YFETLoklBFETLoslQAESZsYTPwj1aFETLpElQAEiZiYTPwnVYFETLpclQAEiZoYTPwn1ZFETLp0lQAEiZuYTPwrVXFETLqovXFETLqUlQAEyZmYTPwrVZFETLqslQAEyZsYTPwr1aFETLrElQAECaiYTPwvVYFETLrclQAECaoYTPwv1ZFETLJvVaFETLGYTPwzVXFETLsMlQAESakYTPwz1YFETLsklQAESaqYTPwzVaFETLs8lQAEiagYTPw31XFETLtUlQAEiBtclQAEiaoYTPw31ZFETLt0lQAEiauYTPw7VXFETLuMlQAEyakYTPw71YFETLuklQAEyaqYTPw7VaFETLu8lQAoPLvElQAECbiYTPw.WYFETLvclQAECboYTPw.2ZFETLv0lQAECbuYDcYE1XAUSbAMSXhEjLgIVPxDlXAMSXkEjLJTibwEzMx41YpwlYuQTP2HWPwjTLAcibAECVIEzMxETLqEVRAcibAESagkTP2HWPw7VXIEzMxEjLEEzMxETLWUjBAcibAEiSEEzMxETLYUTP2HWPwjTQAcibAEiYgUTP2HWPwbVXEEzMxETLnEVQAcibAECVEEzMxETLokVQAcibAoPLpEVQAcibAEiZoUTP2HWPwrVXEEzMxETLqkVQAcibAECagUTP2HWPwvVZEEzMxETLsEVQAcibAESaoUTP2HWPJDiagUTP2HWPw3VZEEzMxETLuEVQAcibAEyaoUTP2HWPw.WXEEzMxETLvkVQAcibAESMzkUXiETMwEzLgIVPxDlBhEjLgIVPyDVYAISMxEWP2HWdAcibAESRwDzMxETLXkTP2HWPwrVXIEzMxETLsEVRAcibAEyagkTP2HWPxTTP2nfbAEyUEEzMxETLNUTP2HWPwjUQAcibAESREEzMxETLlEVQAcibAEyYgUTP2HWPwfVXEEzMxETLXUTP2HWPwjVZJTTP2HWPwnVXEEzMxETLpkVQAcibAEyZgUTP2HWPwrVZEEzMxETLrEVQAcibAECaoUTP2HWPwzVXEEzMxETLsklBEEzMxETLtEVQAcibAEiaoUTP2HWPw7VXEEzMxETLukVQAcibAECbgUTP2HWPw.WZEEzMxETL0PWVgMVP0DWPynPXhEjLgIVPxDlXAMSXkEjL0HWbAcib4EzMxETLIESP2HWPwfURAcibAEyZgkTP2HWPwzVXIEzMxETLuEVRAcibJDjLEEzMxETLWUTP2HWPw3TQAcibAESVEEzMxETLIUTP2HWPwXVXEEzMxETLmEVQAcibAECZgUTP2HWPwfUQAciBxQlZPklXEEzMxETLpEVQAcibAEiZoUTP2HWPwrVXEEzMxETLqkVQAcibAECagUTP2HWPwvVZEEzMxETLsEVQAovMxETLskVQAcibAEiagUTP2HWPw3VZEEzMxETLuEVQAcibAEyaoUTP2HWPw.WXEEzMxETLvkVQAcibAESMzkUXJLVP0DWPyDlXAISXhEjLgIVPyDVYAISMxEWP2HWdAcibAESRwDzMxETLXkTP2HWPwzVXIEzMxETLNUTP2HWPwbkBEEzMxEjLEEzMxETLuEVRAcibAEyZgkTP2HWPwjUQAcibAESREEzMxETLlEVQAcibAEyYgUTP2HWPwfVXEEzMxoPPwfUQAcibAESZoUTP2HWPwnVXEEzMxETLpkVQAcibAEyZgUTP2HWPwrVZEEzMxETLrEVQAcibAECaoUTP2HWPJDSagUTP2HWPwzVZEEzMxETLtEVQAcibAEiaoUTP2HWPw7VXEEzMxETLukVQAcibAECbgUTP2HWPw.WZEEzMxEjBwTCcXEjMwETLxbCcXEjMwETLxbCcXEjMwETLxbCcXEjMwETLxbCcAE1XAUSbyY1YlYlalcTPyX1YlYlXlITb2o.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.YmYFZC4VXso1YuMTagwjZgEkSFsFYPEVZioFarQTag4lZo8FTXkDarE0ZswVZic1PmUVYuE1YCgkYnY1XDcVXJ7FakA2QtklYgAWaDkERmU1QuEFarMVZDQWXsEFYAUSbkUVYpY1XkgVXkEjLg0VPxTFbUUFYjIVPJglZHElahIlBAEiYooUYmQlXAoDZpgzTnIVPwTFbUUFYjUVPNQlXjkVXtIVYAESUmklRnEFRTETLSIlXAESYvUUYjQUPyDlahofXAEiVVgVZjIVPyLEZiETLlklVkcFUAMyTnIVPwT1YZUFbjUVPyD1ahIVPwT1aLYEZjgDYhETLg4lXhETLk8FSJXEZjgDUAESXuI1XAESUFQlXAQyThMVPwX1YkQVYmQFYAoDZpgTXiIlXAEiYmUFYkcFYhEzLgIlXhETLkclVkAmBjQVPyD1ahIVPwT0YooDZggDYhEDRSI1XAEiVVgVZjUVPyLEZiETLlMlUm81YnQlXAIyTOETLkclVkAGUA4DUjofXg8lXhETLkUlUnQ1STEDZpgzThMVPwnkUnkFUA4DUjU1TnkVPwnkUnkFYjEzLSglXAESYjwzYtc1XjIVPxLkXJLVPwTFYLclamMFUAIyThMVPwX1XVc1amgFUAIyTnkVPwX1YkQVYmQVYAMSXiIlXAEiYmUFYkcFUAMSXhESPwTlBmoUYvQlXAMSXuIlXAESUFQUPnMlSTQFZSIVZAESYmwzYscVYjIVPxLkXkETLkcFSm01YkQUPxLkXoEjLNQEYooPPxLSYsoDZpgTPxfiRnoFRnMlSjQVVAMSYvUUYjQFYAMSXuIlXAECMNQFYjIVPwjSYkYEZj8DYhEjLSIlXAISNJnWPxPyLzEVY5EjL4EzLvoVURQTbxDTLwDCam01aoQFQREyRBEWLAESLwT1ZU4VYQA2aRsjPwESPwDSLt81XnMlBiEUXisVaWITRvg1UBEWPwDCNBAmalYVPBEWPwDSMv8VURQDboIULDEWPwDSMvw1ZnA2aDAGZUIEQwETLwTCbgofVuQFQuwFbp8FYDEWPwDSMmc1avw1ZDcVXpwFarQTbAESL0HVZl8VZoQjXjMUZpQTbAESL0nlYpo1XrQTZrAmXJLVaDEWPwDSMSUVZgcFQAsVXggFQwETLwTCZhoFZrQVTlwVYnw1YQEWPwDSMo8lXjYlZQcFYnQlYuEUbAESL0zlBg4VYusFYscVXpUFblQVawETLvfSdPc1XjwFagE1Xi0FYsoVYYUFbj0VbAECL2T2bvklTKITbwDTLvbScyYWbwnPPw.yM0MWbxDTLvbScywVYhcFaiwFakcVZt0VYjwVbAECL2T2bAMCUhUlYmQVawETLvbScyclaM4FYG4Dbk8lXJPTbAECL2T2bv4lTKMjcwETLvbScyAmaRszP1EWPw.yM0MGbtI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRsjBCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScv0lTKovPyYWbAESLwTVauIFbqczbAESL43VXs0lZscTZk8FalI1QAESL4TVauIFbqAUPwf0QhI1ZuUFZGETLwTSYs8lXJzDTAIyQAEiM0DiaksFYYIzag0jZkQTPwDSNqEFZgIlYBoVZpoFYpQTPwDSNtkVSvUFQs4VasEVaDETLwjiao0jBOQzag0jXkEUPwXSM0DlYAICcWEjM4YVZk0FYsMVYKMyYgUVZYIVZKMCcAoWPxjWP3DSNwPmdAciYqQkSlglRnovYH8jRmM1YsgjSkYFZogVXLg1XOETLxPWXkoWPxjWPy.mZUIEQwISPwDSLrcVaukFYDIULKITbwDTLwDSYqUkaJTVTv8lTKITbwDTLwDiauMFZiMVTgM1ZsckPIAGZWITbAESL3HDbtYlYAITbAESL0.2aUIEQvklTwPTbAESL0.mBrsFZv8FQvgVURQTbAESL0.WXZ8FYD8Favo1ajQTbAESL0b1YuAGaqQzYgoFarwFQwETLwTiXoY1aokFQhQ1ToofZDEWPwDSMpYlZpMFaDkFavI1XsQTbAESL0LUYoE1YDEzZgEFZDEWPwDSMnIlZnwFYQYFakgFamEUbAESL0j1aJHFYloVTmQFZjY1aQEWPwDSMsElak81Zj01YgoVYvYFYsEWPw.CN4A0YiQFarEVXiMVaj0lZkkUYvQVawETLvbiB0MGboI0RBEWLAECL2T2b1EWLAECL2T2bwISPw.yM0MGakI1YrMFarU1Yo4VakQFawETLvbScyEzLTIVYlcFYsoPbAECL2T2bm4VStQ1QNAWYuIFQwETLvbScyAmaRszP1EWPw.yM0MGbtI0RCYWbAECL2T2bv4lTKMjcwETLvbScJLGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbiB0MGbsI0RCYWbAECL2TGbsI0RCMmcwETLwDSYs8lXvs1QyETLwjiag0Vap01QoU1arYlXGETLwjSYs8lXvsFTAoPLXcjXhs1akg1QAESL0TVauIVSPEjLGETL1TSLtU1ZjkkPuEVSpUFQAESL4rVXnElXlIjZoolZjoFQAESL43VZJzDbkQTat0Vag0FQAESL43VZM8DQuEVShUVTAEiM0TSXlEjLzcUP1jmYoUVaj01XkszLmEVYokkXoszLzEjdAIiB4EDNwjSLzoWP2X1ZT4jYnoDZmgzSJc1Xm0FRNUlYnkFZgwDZi8TPzfFaWETMzEVY5EjL4EzLvoVURQTbxDTLwnPLrcVaukFYDIULKITbwDTLwDSYqUkakEEbuI0RBEWLAESLw31aig1XiEUXisVaWITRvg1UBEWPwDCNBAmalYVPJHTbAESL0.2aUIEQvklTwPTbAESL0.GaqgFbuQDbnUkTDEWPwDSMvElVuQFQuwFbp8FYDEWPwDSMmc1avw1ZDclBgoFarwFQwETLwTiXoY1aokFQhQ1TooFQwETLwTiZlolZiwFQowFbhMVaDEWPwDSMSUVZgcFQAsVXggFQwETLwnPMnIlZnwFYQYFakgFamEUbAESL0j1ahQlYpE0YjgFYl8VTwETLwTSag4VYusFYscVXpUFblQVawETLvfSdPc1XJPFarEVXiMVaj0lZkkUYvQVawETLvbScyAWZRsjPwESPw.yM0MmcwESPw.yM0MWbxDTLvbScywVYhcFaiwFakclBo4VakQFawETLvbScyEzLTIVYlcFYsEWPw.yM0M2Yt0jajcjSvU1ahQTbAECL2T2bv4lTKMjcwETLvbScyAmaRovRCYWbAECL2T2bv4lTKMjcwETLvbScyAWaRszP1EWPw.yM0MGbsI0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaJH0RCYWbAECL2T2bv0lTKMjcwETLvbScyAWaRszP1EWPw.yM0AWaRszPyYWbAESLwTVauIFbqczbAESL43VXs0lBp01QoU1arYlXGETLwjSYs8lXvsFTAECVGIlXq8VYncTPwDSMk01ah0DTAIyQAEiM0DiaksFYYIzag0jZkQTPwnPL4rVXnElXlIjZoolZjoFQAESL43VZMAWYD0las0VXsQTPwDSNtkVSOQzag0jXkEUPwXSM0DlYAICcWEjM4YVZJTVaj01XkszLmEVYokkXoszLzEjdAISdAgSL4DCc5EzMlsFUNYFZJg1YH8jRmM1YsgjSkYFZogVXLg1XOEDMqklBh0VP0PWXkoWPxjWPynFaqM1XmQTPxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxHTbAEiLxnfPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLxHiPwETLwTSXlI1YrMFYsEzLJDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMiBwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAESL0LWPyDWPwDSMyEzLwETLwTybAMSbAQiLvbSXkoPPxP2UAYSdlkVYsQVaiU1RybVXkkVVhk1RyPWP5EjL4EzLwEzMwETL43FYpwlYuITPwDSStQlXrQTPy.0apokPJDTLwvTSUIjajoFat8VTAcyaqoESBEzMiIVXrYlZBEjL2DWPxbiZs41aOITPzbCaioVXgYlPrMlZgklYQUFYhclBrMFasEzL0DWPyDiXmw1XpEFQAESLkQlXmw1XQETL0zjajIFaDUFYhcFaiQVaAEiMy.0apo0PAcyaqoESCETLwn.aioVXgY1QAMSStQlXrMTPwjSaoYjXmMTPxLyXhEFalo1PAIiLCETL0vTSUMTPwDCSM4FYGEjLyLlXgwlYpMTPJHyLrMlZgklYGETLwbVYiIVXrMTPwjSXrYlZkk1PAESMp0lau8zPAIyMtAGbks1YGETL4DFaloVaocTPyDSYjIlBmQ0QAESM4vTSUITP2H1YrM1UBETLwT1aL0jPAESMloVaoYjPAMSMgwlYpUVZBEzL0DFaloVYoITPyTCSMUkPAovLwrFZjgVTBEjL2.mYqcFYnITPxbSMuslVLMTPwDiajoFal81PAESMoc1XscVYCETL0nVat81SCEzL4nVat81SJLTPxbiZs41avU1QAIyLM4FYpw1QAUSLM4FYhw1PAESMloVaoYzPAISM3HTPwDiXmw1XpEFQAcCSMUkPAIyLtQlBpwlYuITPwTiajoFal8lPAMSNtQlZrY1aBETLwPVZPYkPAMSMloVaoYjPAESLmU1XhEFaBEzLwnVat81SBEjL2nPNrMlZgklYGETLwPVZPY0PAESLM4FYhw1PAMSMpIVZlMVaCEzL0H1YrM1UCEzLhcFaic0PAciXmw1XWMTPwTyaJrlVLMTPy.yPAESLmU1XhEFaCETLwH1YrMlZgcTPzbCbls1Yjg1PAIyL4rFZjgFTDEzLkQlXmQUTL0TUBETL47lBqoESBEzL4zjajIFaBEjLyzVZFI1YBEjLyT1aL0jPAIyLM4FYhwlPAESNM4FYhwlPAciavAWYiclPAESNsklQhovYBEjL3LiajoFal81QjkFTuo1QAIyLkQlXmQ0PAESNyETLw3FbvU1XmMTPwDybAMSMmU1XhEFaCEzLyEzMmU1XJHVXrMTPyjiZhklYi01PAMSL0nVat81SDEjL271ZZwjPAESNtQlZr41aDEzLhcFaickPAESMiIVXrYlZBETL43lBjoFat8FQAESNjkFTVITPwjiZs41avUFQAESNuslVLITPwjSaoYjXmITPwjCYoAkUBEjL3LCTuolVCEzMrMlZgoPZlcjZs41aOcTPynVat81SCw1XpEVXlcTPwDSXrYlZkk1PAESMqgFYnE0PAIyLP8lZZMTPwjiXmw1XWMTPwTSXJvlYpUVZCETL0vTStQ1QAESNP8lZZMTPwjCaioVXoY1QAESLmU1XhEFaCETL4DFaloVYoMTPwTiZs41aOMTPxbiBtAGbks1YGETL4DFaloVaocTPyDSYjI1YTcTPwTSLi41YkMlXBEzLL0TUBEzMp0lau8jPL0jajQTPyzjajoFaDoPPwjiYp0VZFITP2DWPxLSXrYlZkklPAcyYkMlXgwlPAIyLgwlYpUVZBEzL0vTSUITPyDyZnQFZQITPxbCbls1YJPFZBEjL1bSaoYjXmMTPxLiZs41avU1QAMCYoAkUCEjLyzjajIFaCETL43FYpwlYuMTPwTyYkMlXgw1PAMybAciBrMlZgElYCEzMyEzMmU1XhEFaCEzL0TiZs41aOQDSMUkPAciXmw1XWITPyPVZPYkPAMSYuwTSBETL0XlZsklQBoPPxbSbAQyLgwlYpUVZBEzMhcFaickPAMCYoA0apQTPwDSbAcSaoYjXmITPyvTStQFQAESMqgFYnEkPAESNuslVJvjPAIyM0TFYhcFaicTPy71ZZwzPAESLtQlZrY1aCETL0j1Yi01YkMTP2.mYqcFYnMTPynVat81SCETLwLWPxLiBp0lau8zPAIyMp0lauAWYGEjLyzjajoFaGETMwzjajIFaCETL0XlZsklQCEjL0fiPAESMtQlZrY1aBEzMwEjLynPbAESLtQlZrY1aBEzL43FYpwlYuITPwDCYoAkUBEjL2nVat81SBETLzHTPwTSYuwTSBEzMtQlZrY1aBEzMmU1XJHVXrITP2nlXoY1XsITPxbSLp0lau8zPAcSYuwTSCEzMp0lau8zPAcybAcSYuwTSC4FYpwlauAUPyLWPwDybAEiBwLWP2LWP2T1aL0zPAESMyEzLyETL0LWPwDybAMSMvY1ZmQFZCEDM2LWPxbiXmw1XWMTPwjSNzoWP2X1ZT4jYnofRncFROozYicVaH4TYlgVZnEFSnM1SAQSarIlYAUCcWEjM4Y1XlIVYlEzRzf1aIIzRyP2UAYSdlMlYhUlYAsDMJf1aIIzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DzRyP2UAYSdm4lXlIyR1DjBKMCcg0VXjETMwUVYkolYiUFZgQVPxD1YAISYkgzYsoDZpQlXAEyThIVPwTFYLclanEFUAIyThMVPwnESmUVYmofYoQlXAQ0ThIVPwTVYVgFYOQFYAQUPSIVYAESYuglYlMFRncFYhETLgolXhcUPUcVZJgVXHQVYAEyThIVPwT0YJjlRnEFRjQVPwLkXhETLkUlUnQ1SjUVPxLkXoETLZYEZoQlYAoDZpgzTnIVPwnkUnkFYmEzLSg1XAESYkgzYsojBnoFUAEyThIVPwX1XHg1YjIVPyLkXhcUPlMlUm81YnQFYAIyTnIVPwnESmUVYmYVZTETLSIlXAESYjwzYtgVXjofXAIyThIVPwLiYmUFYkcFYlEzLgMlXhETLkYlYhQUPzLkXiETLkYlYhQlXAUVaJglZHMkXhEjLJglZHg1XNQVZJDDMlcVYjU1YjcVPyD1XhIVPwTkQjQVPzLkXiETLlklVkcFYjEzLSg1XAEyMk0lRnoFRAICNJglZHg1XNQlXwDjBxfiSjIFUAIyLk0lRnoFRAICNJglZHg1XNQlXjUVPxfiSjIFYlEjLyTVaJglZHEjL3nDZpgDZi4DYhQFZAICNNo.YhQVZAIyLk0lRnoFRAICNJglZHg1XNQUVAICNNQEYhEjLyTVaJglZHEjL3nDZpgDZi4DUjQVPxfiSTQVYAESNJPmdAcSYvgFYmQlUm0VLJ8DSnMFZjEjLvPmdAcSYmY0Ys8DRnMVPxTCc5EzMZYEZo8DZlg1XHEjLzPmdAcSYlglBo8DZioDZjEjL0PWPgIVP4XVZQU1YVc1aH4jYkglYm8FRSYlZQUlZm81SHg1XnclRm0VPxLyLzETXhETNlkVTko.YnY1SLc1YwLkYpEkYigDZjwzYuozYucFYHEjLyTCcAElXAkiYoEkQHcVawLkYpEUYuwjUnQFRAICMwPWPgIVPJjiYoEUYlc1ancFRm0FSnEFRSYlZQUEZgozYjgDZjEjLyXCc5EzMkclUm81XtUkRng1XtYVXm0FZjMlaUglXnYlBAcCaggVZiwlXnElXAIiZsQmdAcSYmY0YuMlaUoDZnMlalE1YsgFYi4VUnIFZlEzMrEFZoMFahgVXhEjLp0Fc5oPP2T1YVc1ai4VUJgFZi4lYgcVanQ1XtUEZhglYAcCaggVZiwlXnElXAIiZsQmdAciQHcVawfFYNU1Ym0lUnEVLJfDZiETL2DlXzoWP2bVYHc1YJglYm01SAEiMkY1Yug1YTEzLzoWP2bVYHc1YJglYm01SAEiMkY1Yug1YjQVPyPmB5EzMmUFRmclRnY1Ys8TPwXSYlc1ancFYkEzLzoWP2bVYHc1YJglYm01SAEiMZUUYlUFZjIVPxPmdAgCRmclRnofYm01SAQFYvk1YgMDamg1XmE1Pn81atYFbCklZmklYvMjap8FYl81Pm4lYvY1aCQmdAgCRmclRnY1Ys8TPrwVZJXVVBk1YnQVVBYVXJkkPhkVYvkkPt8FYskkPqM1XqkkPzoWP3fzYmoDZlcVaOETYuglXoclPCkFaoclPjY1ZmklBmIDah0VXoclPT4FaoclProFblk1YBQmdAgCRmclRnY1Ys8TPYAGakkFTnQlXuUVaPsFbEUFbP81XmUlYiAUXuoPZoYlYPQlXqwlYoAEc5EzMmUFRmclRnY1Ys8TPwXiVUUlYkgFUAICcAElXAgiQAISM0PWPgIVP3XTPxTSM8fCNJDyL3jiB..."
									}
,
									"fileref" : 									{
										"name" : "Zebra 2.9.3",
										"filename" : "Zebra 2.9.3.maxsnap",
										"filepath" : "~/Documents/Max 9/Snapshots",
										"filepos" : -1,
										"snapshotfileid" : "3622ddff7d873d8e90a670ec2d18be81"
									}

								}
 ]
						}

					}
,
					"text" : "vst~",
					"varname" : "vst~",
					"viewvisibility" : 0
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-2",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 6.0, 131.0, 150.0, 22.0 ],
					"saved_object_attributes" : 					{
						"heap" : 32,
						"ins" : 1,
						"log-null" : 0,
						"outs" : 1,
						"thread" : 104
					}
,
					"text" : "s4m s4m-score-demo.scm"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-2", 0 ],
					"source" : [ "obj-1", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-5", 0 ],
					"source" : [ "obj-10", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-12", 1 ],
					"source" : [ "obj-11", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-12", 0 ],
					"source" : [ "obj-11", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"midpoints" : [ 224.5, 288.999999463558197, 268.5, 288.999999463558197 ],
					"order" : 0,
					"source" : [ "obj-13", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-31", 0 ],
					"order" : 1,
					"source" : [ "obj-13", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-8", 0 ],
					"source" : [ "obj-14", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"source" : [ "obj-15", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-2", 0 ],
					"source" : [ "obj-17", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"source" : [ "obj-18", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"midpoints" : [ 347.5, 288.0, 268.0, 288.0, 268.0, 307.0, 268.5, 307.0 ],
					"source" : [ "obj-19", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-8", 0 ],
					"source" : [ "obj-2", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-20", 0 ],
					"source" : [ "obj-22", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-21", 0 ],
					"source" : [ "obj-22", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-24", 0 ],
					"source" : [ "obj-22", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 1 ],
					"source" : [ "obj-22", 5 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-30", 0 ],
					"source" : [ "obj-22", 6 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-33", 0 ],
					"source" : [ "obj-22", 7 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-49", 0 ],
					"source" : [ "obj-22", 4 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"source" : [ "obj-23", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-34", 0 ],
					"order" : 1,
					"source" : [ "obj-27", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-9", 0 ],
					"order" : 0,
					"source" : [ "obj-27", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-2", 0 ],
					"source" : [ "obj-28", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 1 ],
					"midpoints" : [ 525.5, 319.0, 606.5, 319.0 ],
					"source" : [ "obj-29", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-32", 0 ],
					"source" : [ "obj-30", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"source" : [ "obj-31", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-13", 0 ],
					"midpoints" : [ 525.5, 412.215626001590863, 207.71562758856453, 412.215626001590863, 207.71562758856453, 254.209373259684071, 224.5, 254.209373259684071 ],
					"source" : [ "obj-32", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-35", 1 ],
					"order" : 0,
					"source" : [ "obj-34", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-35", 0 ],
					"order" : 1,
					"source" : [ "obj-34", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-12", 1 ],
					"midpoints" : [ 765.0, 458.159377475967631, 69.5, 458.159377475967631 ],
					"source" : [ "obj-35", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-12", 0 ],
					"midpoints" : [ 751.5, 455.031252429354936, 43.5, 455.031252429354936 ],
					"source" : [ "obj-35", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"source" : [ "obj-38", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-5", 0 ],
					"midpoints" : [ 132.5, 351.871873080730438, 43.5, 351.871873080730438 ],
					"source" : [ "obj-4", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-27", 0 ],
					"source" : [ "obj-40", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-38", 0 ],
					"source" : [ "obj-42", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"midpoints" : [ 408.0, 169.0, 267.0, 169.0 ],
					"source" : [ "obj-47", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 1 ],
					"source" : [ "obj-5", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 0 ],
					"source" : [ "obj-5", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"midpoints" : [ 505.0, 170.0, 267.0, 170.0 ],
					"source" : [ "obj-50", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 0 ],
					"midpoints" : [ 43.5, 317.199998080730438, 43.5, 317.199998080730438 ],
					"source" : [ "obj-6", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-6", 1 ],
					"midpoints" : [ 90.5, 288.199998080730438, 57.0, 288.199998080730438 ],
					"source" : [ "obj-7", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-6", 0 ],
					"midpoints" : [ 43.5, 287.199998080730438, 43.5, 287.199998080730438 ],
					"source" : [ "obj-7", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-7", 2 ],
					"source" : [ "obj-8", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-7", 1 ],
					"source" : [ "obj-8", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-7", 0 ],
					"source" : [ "obj-8", 0 ]
				}

			}
 ],
		"parameters" : 		{
			"obj-11" : [ "live.gain~", "live.gain~", 0 ],
			"obj-35" : [ "live.gain~[1]", "live.gain~", 0 ],
			"obj-5" : [ "vst~", "vst~", 0 ],
			"parameterbanks" : 			{
				"0" : 				{
					"index" : 0,
					"name" : "",
					"parameters" : [ "-", "-", "-", "-", "-", "-", "-", "-" ]
				}

			}
,
			"inherited_shortname" : 1
		}
,
		"dependency_cache" : [ 			{
				"name" : "Zebra 2.9.3.maxsnap",
				"bootpath" : "~/Documents/Max 9/Snapshots",
				"patcherrelativepath" : "../../../Max 9/Snapshots",
				"type" : "mx@s",
				"implicit" : 1
			}
, 			{
				"name" : "s4m.mxo",
				"type" : "iLaX"
			}
 ],
		"autosave" : 0
	}

}

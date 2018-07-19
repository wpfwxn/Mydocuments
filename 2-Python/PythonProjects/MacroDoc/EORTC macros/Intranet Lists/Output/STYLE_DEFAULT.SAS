define style Styles.Journal;
   parent = styles.default;
   style fonts /
      'docFont' = ("<sans-serif>, Helvetica, Helv",2)
      'headingFont' = ("<sans-serif>, Helvetica, Helv",2,italic)
      'headingEmphasisFont' = ("<sans-serif>, Helvetica, Helv",2,bold italic)
      'FixedFont' = ("<monospace>, Courier",2)
      'BatchFixedFont' = ("SAS Monospace, <monospace>, Courier, monospace",2)
      'FixedHeadingFont' = ("<monospace>, Courier",2,italic)
      'FixedStrongFont' = ("<monospace>, Courier",2,bold)
      'FixedEmphasisFont' = ("<sans-serif>, Helvetica, Helv",2,bold italic)
      'EmphasisFont' = ("<sans-serif>, Helvetica, Helv",2,italic)
      'StrongFont' = ("<sans-serif>, Helvetica, Helv",2,bold italic)
      'TitleFont' = ("<sans-serif>, Helvetica, Helv",2,bold italic)
      'TitleFont2' = ("<sans-serif>, Helvetica, Helv",2,bold italic)
      'SASTitleFont' = ("<sans-serif>, Helvetica, Helv",2,bold italic);
   class GraphFonts /
      'NodeDetailFont' = ("<sans-serif>, <MTsans-serif>",7pt)
      'NodeInputLabelFont' = ("<sans-serif>, <MTsans-serif>",9pt)
      'NodeLabelFont' = ("<sans-serif>, <MTsans-serif>",9pt)
      'NodeTitleFont' = ("<sans-serif>, <MTsans-serif>",9pt)
      'GraphTitleFont' = ("<sans-serif>, <MTsans-serif>",11pt)
      'GraphTitle1Font' = ("<sans-serif>, <MTsans-serif>",14pt)
      'GraphFootnoteFont' = ("<sans-serif>, <MTsans-serif>",10pt)
      'GraphLabel2Font' = ("<sans-serif>, <MTsans-serif>",10pt)
      'GraphLabelFont' = ("<sans-serif>, <MTsans-serif>",10pt)
      'GraphUnicodeFont' = ("<MTsans-serif-unicode>",9pt)
      'GraphValueFont' = ("<sans-serif>, <MTsans-serif>",9pt)
      'GraphDataFont' = ("<sans-serif>, <MTsans-serif>",7pt)
      'GraphAnnoFont' = ("<sans-serif>, <MTsans-serif>",10pt);
   class colors /
      'fg' = cx000000
      'fg2' = cx000000
      'fgA1' = cx000000
      'bgA1' = cxFFFFFF
      'bg2' = cxFFFFFF
      'fg3' = cx000000
      'bg3' = cxFFFFFF
      'fg4' = cx000000
      'bg4' = cxFFFFFF
      'bg5' = cxFFFFFF
      'link1' = cx004488
      'link2' = cx0066AA
      'titlefg' = _undef_
      'contentfg' = cx000000
      'contentbg' = cxFFFFFF
      'docbg' = cxFFFFFF;
   class GraphColors /
      'goffill' = cx666666
      'gofill' = cxf6f6f6
      'govbline' = cx848b93
      'gobline' = cx848b93
      'gncdata12' = cx000000
      'gncdata11' = cx000000
      'gncdata10' = cx000000
      'gncdata9' = cx000000
      'gncdata8' = cx000000
      'gncdata7' = cx000000
      'gncdata6' = cx000000
      'gncdata5' = cx000000
      'gncdata4' = cx000000
      'gncdata3' = cx000000
      'gncdata2' = cx000000
      'gncdata1' = cx000000
      'gncdata' = cx000000
      'gndata12' = cx8F8F8F
      'gndata11' = cxF1F1F1
      'gndata10' = cxABABAB
      'gndata9' = cxBDBDBD
      'gndata8' = cxCFCFCF
      'gndata7' = cxD9D9D9
      'gndata6' = cx989898
      'gndata5' = cxE9E9E9
      'gndata4' = cxA1A1A1
      'gndata3' = cxB4B4B4
      'gndata2' = cxC6C6C6
      'gndata1' = cxE2E2E2
      'gndata' = cxE2E2E2
      'gnclink' = cx969696
      'gnlink' = cxC4C4C4
      'gntext' = cx111111
      'gblockheader' = cxd4d4d4
      'gcphasebox' = cx000000
      'gphasebox' = cxBBBBBB
      'gczonec' = cxC9C9C9
      'gzonec' = cxD8D8D8
      'gczoneb' = cxD8D8D8
      'gzoneb' = cxE1E1E1
      'gzonea' = cxEBEBEB
      'gcunder' = cxCCCCCC
      'gunder' = cxCCCCCC
      'gcover' = cxF1F1F1
      'gover' = cxF1F1F1
      'gcfinal' = cx777777
      'gfinal' = cx777777
      'gcinitial' = cx222222
      'ginitial' = cx222222
      'gcother' = cxF1F1F1
      'gother' = cxF1F1F1
      'gcmiss' = cx545454
      'gmiss' = cx000000
      'gablock' = cxE0E0E0
      'gblock' = cxF2F2F2
      'gcclipping' = cx000000
      'gclipping' = cxD2D2D2
      'gcstars' = cx000000
      'gstars' = cxD2D2D2
      'gcruntest' = cxA3A3A3
      'gruntest' = cxDDDDDD
      'gccontrollim' = cxC2C2C2
      'gcontrollim' = cxF0F0F0
      'gcerror' = cx000000
      'gerror' = cxA0A0A0
      'gcpredictlim' = cx000000
      'gpredictlim' = cxC8C8C8
      'gcpredict' = cx000000
      'gpredict' = cx000000
      'gcconfidence2' = cx000000
      'gcconfidence' = cx000000
      'gconfidence2' = cxA8A8A8
      'gconfidence' = cxC8C8C8
      'gcfit2' = cx000000
      'gcfit' = cx000000
      'gfit2' = cx000000
      'gfit' = cx000000
      'gcoutlier' = cx000000
      'goutlier' = cxA0A0A0
      'gcdata' = cx000000
      'gdata' = cxD2D2D2
      'ginsetheader' = colors('docbg')
      'ginset' = cxFFFFFF
      'greferencelines' = cx808080
      'gheader' = colors('docbg')
      'gramp3cend' = cx5F5F5F
      'gramp3cneutral' = cxA7A7A7
      'gramp3cstart' = cxF0F0F0
      'gramp2cend' = cx5F5F5F
      'gramp2cstart' = cxF0F0F0
      'gconramp3cend' = cx000000
      'gconramp3cneutral' = cx777777
      'gconramp3cstart' = cxC4C4C4
      'gconramp2cend' = cx5F5F5F
      'gconramp2cstart' = cxF0F0F0
      'gtext' = cx000000
      'glabel' = cx000000
      'gborderlines' = cx000000
      'goutlines' = cx000000
      'gmgrid' = cxF2F2F2
      'ggrid' = cxECECEC
      'gaxis' = cx000000
      'gshadow' = cx000000
      'glegend' = cxFFFFFF
      'gfloor' = cxCCCCCC
      'gwalls' = cxFFFFFF
      'gckran5' = cx000000
      'gkran5' = cxe1e1e1
      'gckran4' = cx000000
      'gkran4' = cxbfbfbf
      'gckran3' = cx000000
      'gkran3' = cx828282
      'gckran2' = cx000000
      'gkran2' = cx454545
      'gckran1' = cx000000
      'gkran1' = cx080808
      'gcdata12' = cx000000
      'gcdata11' = cx000000
      'gcdata10' = cx000000
      'gcdata9' = cx000000
      'gcdata8' = cx000000
      'gcdata7' = cx000000
      'gcdata6' = cx000000
      'gcdata5' = cx000000
      'gcdata4' = cx000000
      'gcdata3' = cx000000
      'gcdata2' = cx000000
      'gcdata1' = cx000000
      'gdata11' = CXe1e1e1
      'gdata5' = CXcfcfcf
      'gdata1' = CXbfbfbf
      'gdata7' = CXababab
      'gdata8' = CX969696
      'gdata2' = CX828282
      'gdata9' = CX6e6e6e
      'gdata3' = CX595959
      'gdata10' = CX454545
      'gdata4' = CX303030
      'gdata6' = CX1c1c1c
      'gdata12' = CX080808;
   style Container;
   style Index from Container /
      color = colors('fg');
   style Contents from Document /
      marginleft = 8
      marginright = 8
      backgroundcolor = colors('bg2')
      pagebreakhtml = html('break')
      tagattr = " onload=""expandAll()"""
      liststyletype = "decimal";
   style Pages from Document /
      marginleft = 8
      marginright = 8
      backgroundcolor = colors('bg2')
      pagebreakhtml = html('break')
      tagattr = " onload=""expandAll()"""
      liststyletype = "decimal";
   style Date from Container /
      color = colors('fg3')
      width = 100%;
   style BodyDate from Date;
   style IndexItem from Container /
      liststyletype = NONE
      listentryanchor = on
      prehtml = html('fake bullet')
      font = Fonts('docFont');
   style ContentFolder from IndexItem /
      listentryanchor = off
      prehtml = html('prehtml flyover bullet')
      posthtml = html('posthtml flyover')
      font = Fonts('docFont');
   style IndexProcName from Index /
      pretext = text('prefix1')
      posttext = text('suffix1')
      prehtml = html('prehtml flyover')
      posthtml = html('posthtml flyover')
      liststyletype = "decimal"
      listentryanchor = off
      font = Fonts('FixedEmphasisFont');
   style IndexTitle from Index /
      prehtml = html('expandAll')
      posthtml = html('posthtml flyover line')
      font = Fonts('FixedStrongFont');
   style SysTitleAndFooterContainer from Container /
      borderwidth = 0
      borderspacing = 0
      cellpadding = 1
      width = 100%
      frame = VOID
      rules = NONE
      font = Fonts('TitleFont2')
      prehtml = "<!--BEGINTITLE-->"
      posthtml = "<!--ENDTITLE-->";
   style TitleAndNoteContainer from Container /
      borderwidth = 0
      borderspacing = 1
      cellpadding = 1
      width = 100%
      frame = VOID
      rules = NONE
      prehtml = "<!--BEGINTITLE1-->"
      posthtml = "<!--ENDTITLE1-->";
   style TitlesAndFooters from Container /
      color = colors('fg')
      font = Fonts('TitleFont2');
   style BylineContainer from Container /
      borderwidth = 0
      borderspacing = 1
      cellpadding = 1
      width = 100%
      frame = VOID
      rules = NONE;
   style SystemTitle from TitlesAndFooters /
      font = Fonts('TitleFont');
   style SystemFooter from TitlesAndFooters /
      font = Fonts('TitleFont');
   style Byline from TitlesAndFooters /
      color = colors('fg2')
      font = fonts('TitleFont');
   style Parskip from TitlesAndFooters /
      font = fonts('headingFont')
      cellpadding = 0
      borderspacing = 0;
   style Continued from TitlesAndFooters
      "Controls continued flag" /
      font = fonts('headingFont')
      cellpadding = 0
      borderspacing = 0
      pretext = text('continued')
      width = 100%
      textalign = left;
   style ProcTitle from TitlesAndFooters /
      font = fonts('TitleFont2');
   style Output from Container /
      bordercolor = colors('fgA1')
      borderwidth = 1
      borderspacing = 0
      cellpadding = 7
      frame = HSIDES
      rules = GROUPS
      backgroundcolor = colors('bgA1')
      bordercollapse = separate
      borderstyle = solid;
   style Table from Output /
      prehtml = "<!--BEGINTABLE-->"
      posthtml = "<!--ENDTABLE-->";
   style Batch from Output
      "Batch (capture) Output style." /
      color = colors('fg')
      font = fonts('BatchFixedFont')
      padding = 7;
   style Graph from Output;
   style Note from Container /
      prehtml = "<!--BEGINNOTE-->"
      posthtml = "<!--ENDNOTE-->";
   style NoteBanner from Note /
      pretext = text('Note Banner')
      font = fonts('StrongFont');
   style NoteContent from Note
      "Controls the contents for NOTE:s." /
      textalign = left;
   style NoteContentFixed from NoteContent
      "Controls the contents for NOTE:s. Fixed font." /
      font = fonts('FixedFont');
   style WarnBanner from Note
      "Controls the banner for WARNING:s." /
      pretext = text('Warn Banner');
   style WarnContent from Note
      "Controls the contents of WARNING:s." /
      textalign = left;
   style WarnContentFixed from WarnContent
      "Controls the contents for WARNING:s. Fixed font." /
      font = fonts('FixedFont');
   style ErrorBanner from Note
      "Controls the banner for ERROR:s." /
      pretext = text('Error Banner');
   style ErrorContent from Note
      "Controls the contents of ERROR:s." /
      textalign = left;
   style ErrorContentFixed from ErrorContent
      "Controls the contents for ERROR:s. Fixed font." /
      font = fonts('FixedFont');
   style FatalBanner from Note
      "Controls the banner for FATAL:s." /
      pretext = text('Fatal Banner');
   style FatalContent from Note
      "Controls the contents of FATAL:s." /
      textalign = left;
   style FatalContentFixed from FatalContent
      "Controls the contents for FATAL:s. Fixed font." /
      font = fonts('FixedFont');
   style Data from Cell /
      backgroundcolor = colors('bg3')
      color = colors('fg3')
      font = Fonts('DocFont');
   style LineContent from Cell /
      backgroundcolor = colors('bg3')
      color = colors('fg3')
      font = Fonts('DocFont');
   style DataEmphasis from Data /
      font = fonts('EmphasisFont');
   style DataEmphasisFixed from DataEmphasis /
      font = fonts('FixedEmphasisFont');
   style DataStrong from Data /
      font = fonts('StrongFont');
   style HeadersAndFooters from Cell /
      backgroundcolor = colors('bg2')
      color = colors('fg2')
      font = fonts('HeadingFont');
   style Caption from HeadersAndFooters;
   style HeaderEmphasis from Header /
      font = fonts('EmphasisFont');
   style HeaderStrong from Header /
      font = fonts('StrongFont');
   style GraphData1 from GraphData1 /
      markersymbol = "circle";
   style GraphData2 from GraphData2 /
      markersymbol = "plus";
   style GraphData3 from GraphData3 /
      markersymbol = "X";
   style GraphData4 from GraphData4 /
      markersymbol = "triangle";
   style GraphData5 from GraphData5 /
      markersymbol = "tilde";
   style GraphData6 from GraphData6 /
      markersymbol = "ibeam";
   style GraphData7 from GraphData7 /
      markersymbol = "square";
   style GraphData8 from GraphData8 /
      markersymbol = "asterisk";
   style GraphData9 from GraphData9 /
      markersymbol = "diamond";
   style GraphData10 from GraphData10 /
      markersymbol = "union";
   style GraphData11 from GraphData11 /
      markersymbol = "hash";
   style GraphData12 from GraphData12 /
      markersymbol = "tack";
   style GraphData13 from GraphComponent /
      markersymbol = "homedown";
   style GraphData14 from GraphComponent /
      markersymbol = "greaterthan";
   style GraphData15 from GraphComponent /
      markersymbol = "arrow";
   class KPIRange1 /
      markersize = 7px
      markersymbol = "square"
      linethickness = 1px
      linestyle = 20
      contrastcolor = GraphColors('gckran1')
      color = GraphColors('gkran1');
   class KPIRange2 /
      markersize = 7px
      markersymbol = "asterisk"
      linethickness = 1px
      linestyle = 21
      contrastcolor = GraphColors('gckran2')
      color = GraphColors('gkran2');
   class KPIRange3 /
      markersize = 7px
      markersymbol = "diamond"
      linethickness = 1px
      linestyle = 41
      contrastcolor = GraphColors('gckran3')
      color = GraphColors('gkran3');
   class KPIRange4 /
      markersize = 7px
      markersymbol = "hash"
      linethickness = 1px
      linestyle = 42
      contrastcolor = GraphColors('gckran4')
      color = GraphColors('gkran4');
   class KPIRange5 /
      markersize = 7px
      markersymbol = "homedown"
      linethickness = 1px
      linestyle = 2
      contrastcolor = GraphColors('gckran5')
      color = GraphColors('gkran5');
   style GraphHistogram from GraphComponent /
      displayopts = "fill outline";
   style GraphEllipse from GraphComponent /
      displayopts = "outline";
   style GraphBand from GraphComponent /
      displayopts = "fill";
   style GraphBox from GraphComponent /
      displayopts = "fill caps median mean outliers"
      connect = "mean"
      capstyle = "serif";
   style GraphContour from GraphContour /
      displayopts = "LabeledLine";
   class NodeTitleText /
      font = GraphFonts('NodeTitleFont')
      color = GraphColors('gntext');
   class NodeLabelText /
      font = GraphFonts('NodeLabelFont')
      color = GraphColors('gntext');
   class NodeInputLabelText /
      font = GraphFonts('NodeInputLabelFont')
      color = GraphColors('gntext');
   class NodeDetailText /
      font = GraphFonts('NodeDetailFont')
      color = GraphColors('gntext');
   class NodeLink /
      linethickness = 1px
      linestyle = 1
      contrastcolor = GraphColors('gnclink')
      color = GraphColors('gnlink');
   class OverviewBorderLines /
      linethickness = 1px
      contrastcolor = GraphColors('gobline');
   class OverviewViewportBorderLines /
      linethickness = 2px
      contrastcolor = GraphColors('govbline');
   class OverviewFill /
      color = GraphColors('gofill');
   class OverviewFilterFill /
      transparency = 0.6
      color = GraphColors('goffill');
   class GraphDataNodeDefault /
      endcolor = GraphColors('gramp3cend')
      neutralcolor = GraphColors('gramp3cneutral')
      startcolor = GraphColors('gramp3cstart')
      linethickness = 1px
      linestyle = 1
      contrastcolor = GraphColors('gncdata')
      color = GraphColors('gndata');
   class GraphDataNode1 /
      contrastcolor = GraphColors('gncdata1')
      color = GraphColors('gndata1');
   class GraphDataNode2 /
      contrastcolor = GraphColors('gncdata2')
      color = GraphColors('gndata2');
   class GraphDataNode3 /
      contrastcolor = GraphColors('gncdata3')
      color = GraphColors('gndata3');
   class GraphDataNode4 /
      contrastcolor = GraphColors('gncdata4')
      color = GraphColors('gndata4');
   class GraphDataNode5 /
      contrastcolor = GraphColors('gncdata5')
      color = GraphColors('gndata5');
end;

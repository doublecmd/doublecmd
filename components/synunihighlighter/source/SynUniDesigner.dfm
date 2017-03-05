object fmDesigner: TfmDesigner
  Left = 321
  Height = 475
  Top = 127
  Width = 640
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'TSynUniDesigner'
  ClientHeight = 475
  ClientWidth = 640
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Icon.Data = {
    7E03000000000100010010100000180018006803000016000000280000001000
    0000200000000100180000000000400300000000000000000000000000010000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000003930323930323930323930323930
    3239303239303239303239303239303239303239303239303239303239303239
    30323930320AD5F00AD5F00AD5F03930320AD5F00AD5F00AD5F08D6767393032
    8D67673930328D67678D67678D67673930323930320AD5F00AD5F00AD5F03930
    320AD5F00AD5F08D6767000000CFA7A80000008D67673930328D67678D676739
    30323930320AD5F00AD5F00AD5F03930320AD5F08D6767CDA2A4CDA2A4C99C9E
    CDA2A4CFA7A83333338D67678D67673930323930323930323930323930323930
    328A65668D6767D6BEBF8D67670000008D6767CDA2A43930323333338D676739
    30323930321518EC1518EC1518EC3930328D6767E3D8D8E3D8D8000000F3C9CB
    000000C99C9ECFA7A83333338D67673930323930321518EC1518EC1518EC3930
    328D67678D6767F5F0F08D67670000008D6767CDA2A43930323333338D676739
    30323930321518EC1518EC1518EC393032FF99FF8D6767FFFFFFF5F0F0E3D8D8
    D6BEBFCDA2A43333338D67678D67673930323930323930323930323930323930
    323930328D67678D6767000000E3D8D80000003333333333338D67678D676739
    3032393032FF100AFF100AFF100A393032FFC333FFC333FFC3338D67678D6767
    3333338D67678D67678D67678D6767393032393032FF100AFF100AFF100A3930
    32FFC333FFC333FFC3333930320CE3170CE3170CE3173930328D67678D676739
    3032393032393032393032393032393032393032393032393032393032393032
    3930323930323930323930323930323930323930328D67678D67678D67678D67
    678D67678D67678D67678D67678D67678D67678D67678D67678D67678D676739
    30323930328D67678D67678D67678D67678D67678D67678D67678D67678D6767
    8D67678D6767FFFFFF8D6767FFFFFF3930323930323930323930323930323930
    3239303239303239303239303239303239303239303239303239303239303239
    3032000032390000323900003239000032390000323900003239000032390000
    3239000032390000323900003239000032390000323900003239000032390000
    3239
  }
  KeyPreview = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  Position = poDesktopCenter
  LCLVersion = '0.9.30'
  object SplitterBottom: TSplitter
    Cursor = crVSplit
    Left = 0
    Height = 3
    Top = 229
    Width = 640
    Align = alTop
    MinSize = 17
    OnCanResize = SplitterBottomCanResize
    ResizeAnchor = akTop
  end
  object SplitterButtons: TSplitter
    Cursor = crVSplit
    Left = 0
    Height = 3
    Top = 417
    Width = 640
    Align = alBottom
    MinSize = 35
    OnCanResize = SplitterCannotResize
    ResizeAnchor = akBottom
  end
  object StatusBar: TStatusBar
    Left = 0
    Height = 20
    Top = 455
    Width = 640
    Panels = <    
      item
        Alignment = taCenter
        Width = 50
      end    
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object pTop: TPanel
    Tag = -1
    Left = 0
    Height = 229
    Top = 0
    Width = 640
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    ClientHeight = 229
    ClientWidth = 640
    TabOrder = 1
    object SplitterLeft: TSplitter
      Left = 185
      Height = 227
      Top = 1
      Width = 3
      MinSize = 122
    end
    object SplitterRight: TSplitter
      Left = 536
      Height = 227
      Top = 1
      Width = 3
      Align = alRight
      OnCanResize = SplitterCannotResize
      ResizeAnchor = akRight
    end
    object pLeft: TPanel
      Left = 1
      Height = 227
      Top = 1
      Width = 184
      Align = alLeft
      BevelInner = bvLowered
      BevelOuter = bvNone
      ClientHeight = 227
      ClientWidth = 184
      Constraints.MinWidth = 122
      TabOrder = 0
      object Bevel1: TBevel
        Left = 1
        Height = 1
        Top = 16
        Width = 182
        Align = alTop
        Style = bsRaised
      end
      object pLeftParentCapt: TPanel
        Left = 1
        Height = 15
        Top = 1
        Width = 182
        Align = alTop
        BevelOuter = bvNone
        ClientHeight = 15
        ClientWidth = 182
        Color = clActiveCaption
        Font.Color = clCaptionText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        PopupMenu = popPanels
        TabOrder = 0
        object lbRootMenu: TLabel
          Left = 0
          Height = 15
          Hint = 'Tree Menu'
          Top = 0
          Width = 17
          Align = alLeft
          Caption = 'u'
          Font.Color = clBtnFace
          Font.Height = -15
          Font.Name = 'Marlett'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = lbRootMenuClick
          OnMouseEnter = LabelMouseEnter
          OnMouseLeave = LabelMouseLeave
          OnContextPopup = LabelContextPopup
        end
        object pLeftCapt: TPanel
          Left = 17
          Height = 15
          Top = 0
          Width = 165
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Rules'' Tree'
          Color = clActiveCaption
          ParentColor = False
          TabOrder = 0
        end
      end
      object pTree: TPanel
        Left = 4
        Height = 203
        Top = 20
        Width = 176
        Anchors = [akTop, akLeft, akRight, akBottom]
        BevelOuter = bvNone
        ClientHeight = 203
        ClientWidth = 176
        TabOrder = 1
        object Tree: TTreeView
          Left = 0
          Height = 203
          Top = 0
          Width = 176
          Align = alClient
          DefaultItemHeight = 16
          HideSelection = False
          Images = listRules
          Indent = 19
          MultiSelectStyle = [msControlSelect, msShiftSelect]
          RowSelect = True
          TabOrder = 0
          OnChange = TreeChange
          OnClick = TreeClick
          OnEdited = TreeEdited
          OnKeyDown = TreeKeyDown
          OnMouseDown = TreeMouseDown
          OnMouseUp = TreeMouseUp
          Options = [tvoAutoItemHeight, tvoKeepCollapsedNodes, tvoRowSelect, tvoShowButtons, tvoShowLines, tvoShowRoot, tvoToolTips, tvoThemedDraw]
        end
      end
    end
    object pRight: TPanel
      Left = 539
      Height = 227
      Top = 1
      Width = 100
      Align = alRight
      BevelInner = bvLowered
      BevelOuter = bvNone
      ClientHeight = 227
      ClientWidth = 100
      TabOrder = 2
      object Bevel2: TBevel
        Left = 1
        Height = 1
        Top = 16
        Width = 98
        Align = alTop
        Style = bsRaised
      end
      object pRightCapt: TPanel
        Left = 1
        Height = 15
        Top = 1
        Width = 98
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Attributes'
        Color = clActiveCaption
        Font.Color = clCaptionText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        PopupMenu = popPanels
        TabOrder = 0
      end
      object pAttri: TPanel
        Left = 1
        Height = 209
        Top = 17
        Width = 98
        Align = alClient
        BevelOuter = bvNone
        ClientHeight = 209
        ClientWidth = 98
        TabOrder = 1
        object PageControl1: TPageControl
          Left = 0
          Height = 209
          Top = 0
          Width = 98
          ActivePage = TabSheet1
          Align = alClient
          MultiLine = True
          TabIndex = 0
          TabOrder = 0
          Options = [nboMultiLine]
          object TabSheet1: TTabSheet
            Caption = 'Style'
            ClientHeight = 183
            ClientWidth = 90
            TabVisible = False
            object Bevel6: TBevel
              Tag = 118
              Left = 0
              Height = 2
              Top = 43
              Width = 90
            end
            object Label2: TLabel
              Left = 2
              Height = 14
              Top = 0
              Width = 64
              Caption = 'Choose style:'
              Enabled = False
              ParentColor = False
            end
            object Label4: TLabel
              Left = 2
              Height = 14
              Top = 48
              Width = 65
              Caption = 'Change style:'
              ParentColor = False
            end
            object chStrikeOut: TCheckBox
              Left = 3
              Height = 17
              Top = 157
              Width = 64
              Caption = 'Strike&Out'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsStrikeOut]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 0
            end
            object chUnderline: TCheckBox
              Left = 3
              Height = 17
              Top = 141
              Width = 65
              Caption = '&Underline'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsUnderline]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 1
            end
            object chItalic: TCheckBox
              Left = 3
              Height = 17
              Top = 125
              Width = 42
              Caption = '&Italic'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsItalic]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 2
            end
            object chBold: TCheckBox
              Left = 3
              Height = 17
              Top = 109
              Width = 45
              Caption = '&Bold'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 3
            end
            object pForeColorBox: TPanel
              Left = 2
              Height = 23
              Top = 82
              Width = 41
              ClientHeight = 23
              ClientWidth = 41
              TabOrder = 4
              object pForeColor: TPanel
                Left = 2
                Height = 18
                Top = 2
                Width = 28
                BevelInner = bvLowered
                BevelOuter = bvNone
                TabOrder = 0
                OnClick = PanelColorChange
                OnMouseUp = pColorMouseUp
              end
              object pForeColorArrow: TPanel
                Left = 31
                Height = 18
                Top = 2
                Width = 8
                BevelOuter = bvNone
                Caption = 'u'
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Marlett'
                ParentFont = False
                TabOrder = 1
                OnMouseUp = pColorArrowMouseUp
              end
            end
            object pBackColorBox: TPanel
              Left = 48
              Height = 22
              Top = 82
              Width = 41
              ClientHeight = 22
              ClientWidth = 41
              TabOrder = 5
              object pBackColor: TPanel
                Left = 2
                Height = 18
                Top = 2
                Width = 28
                BevelInner = bvLowered
                BevelOuter = bvNone
                TabOrder = 0
                OnClick = PanelColorChange
                OnMouseUp = pColorMouseUp
              end
              object pBackColorArrow: TPanel
                Left = 31
                Height = 18
                Top = 2
                Width = 8
                BevelOuter = bvNone
                Caption = 'u'
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Marlett'
                ParentFont = False
                TabOrder = 1
                OnMouseUp = pColorArrowMouseUp
              end
            end
            object chForeground: TCheckBox
              Left = 2
              Height = 17
              Top = 64
              Width = 42
              Caption = 'D&FG'
              Checked = True
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              State = cbChecked
              TabOrder = 6
            end
            object chBackground: TCheckBox
              Left = 48
              Height = 17
              Top = 64
              Width = 43
              Caption = 'DB&G'
              Checked = True
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              State = cbChecked
              TabOrder = 7
            end
            object cbStyle: TComboBox
              Left = 0
              Height = 21
              Top = 16
              Width = 90
              Color = clBtnFace
              DropDownCount = 16
              Enabled = False
              ItemHeight = 13
              Items.Strings = (
                'Root'
                'Comments'
                'Strings'
                'Reserved words'
                'Statements'
                'Numbers'
                'Symbols'
                'Directives'
                'Types'
                'Variables'
                'Functions'
                ''
                'New style...'
              )
              OnChange = AttributesChanged
              Style = csDropDownList
              TabOrder = 8
            end
          end
          object TabSheet2: TTabSheet
            Caption = 'Default'
            ClientHeight = 0
            ClientWidth = 0
            ImageIndex = 1
            TabVisible = False
            object CheckBox1: TCheckBox
              Left = 3
              Height = 17
              Top = 93
              Width = 86
              Caption = 'Strike&Out'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsStrikeOut]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 0
            end
            object CheckBox2: TCheckBox
              Left = 3
              Height = 17
              Top = 77
              Width = 86
              Caption = '&Underline'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsUnderline]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 1
            end
            object CheckBox3: TCheckBox
              Left = 3
              Height = 17
              Top = 61
              Width = 86
              Caption = '&Italic'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsItalic]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 2
            end
            object CheckBox4: TCheckBox
              Left = 3
              Height = 17
              Top = 45
              Width = 86
              Caption = '&Bold'
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              ParentFont = False
              TabOrder = 3
            end
            object Panel2: TPanel
              Left = 2
              Height = 23
              Top = 18
              Width = 41
              ClientHeight = 23
              ClientWidth = 41
              TabOrder = 4
              object Panel3: TPanel
                Left = 2
                Height = 18
                Top = 2
                Width = 28
                BevelInner = bvLowered
                BevelOuter = bvNone
                TabOrder = 0
                OnClick = PanelColorChange
                OnMouseUp = pColorMouseUp
              end
              object Panel4: TPanel
                Left = 31
                Height = 18
                Top = 2
                Width = 8
                BevelOuter = bvNone
                Caption = 'u'
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Marlett'
                ParentFont = False
                TabOrder = 1
                OnMouseUp = pColorArrowMouseUp
              end
            end
            object Panel5: TPanel
              Left = 48
              Height = 22
              Top = 18
              Width = 41
              ClientHeight = 22
              ClientWidth = 41
              TabOrder = 5
              object Panel6: TPanel
                Left = 2
                Height = 18
                Top = 2
                Width = 28
                BevelInner = bvLowered
                BevelOuter = bvNone
                TabOrder = 0
                OnClick = PanelColorChange
                OnMouseUp = pColorMouseUp
              end
              object Panel7: TPanel
                Left = 31
                Height = 18
                Top = 2
                Width = 8
                BevelOuter = bvNone
                Caption = 'u'
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Marlett'
                ParentFont = False
                TabOrder = 1
                OnMouseUp = pColorArrowMouseUp
              end
            end
            object CheckBox5: TCheckBox
              Left = 2
              Height = 17
              Top = 0
              Width = 41
              Caption = 'D&FG'
              Checked = True
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              State = cbChecked
              TabOrder = 6
            end
            object CheckBox6: TCheckBox
              Left = 48
              Height = 17
              Top = 0
              Width = 41
              Caption = 'DB&G'
              Checked = True
              OnClick = AttributesChanged
              OnMouseDown = CheckBoxMouseDown
              State = cbChecked
              TabOrder = 7
            end
            object CheckBox7: TCheckBox
              Left = 0
              Height = 17
              Top = 152
              Width = 90
              Caption = 'Save defaults'
              TabOrder = 8
            end
            object Button1: TButton
              Left = 0
              Height = 21
              Top = 171
              Width = 90
              Caption = 'Get from Style'
              TabOrder = 9
            end
          end
        end
      end
    end
    object pMiddle: TPanel
      Left = 188
      Height = 227
      Top = 1
      Width = 348
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      ClientHeight = 227
      ClientWidth = 348
      TabOrder = 1
      OnResize = pMiddleResize
      object Bevel4: TBevel
        Left = 1
        Height = 1
        Top = 16
        Width = 346
        Align = alTop
        Style = bsRaised
      end
      object pMiddleParentCapt: TPanel
        Left = 1
        Height = 15
        Top = 1
        Width = 346
        Align = alTop
        BevelOuter = bvNone
        ClientHeight = 15
        ClientWidth = 346
        Color = clActiveCaption
        Font.Color = clCaptionText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        PopupMenu = popPanels
        TabOrder = 0
        object lbPropBack: TLabel
          Left = 0
          Height = 15
          Hint = 'Back'
          Top = 0
          Width = 17
          Align = alLeft
          Caption = '3'
          Font.Color = clBtnFace
          Font.Height = -15
          Font.Name = 'Marlett'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = lbPropBackClick
          OnMouseEnter = LabelMouseEnter
          OnMouseLeave = LabelMouseLeave
          OnContextPopup = LabelContextPopup
        end
        object lbRuleMenu: TLabel
          Left = 329
          Height = 15
          Hint = 'Rule Menu'
          Top = 0
          Width = 17
          Align = alRight
          Caption = 'u'
          Font.Color = clBtnFace
          Font.Height = -15
          Font.Name = 'Marlett'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          OnClick = lbRuleMenuClick
          OnMouseEnter = LabelMouseEnter
          OnMouseLeave = LabelMouseLeave
          OnContextPopup = LabelContextPopup
        end
        object pMiddleCapt: TPanel
          Left = 17
          Height = 15
          Top = 0
          Width = 312
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Properties'
          Color = clActiveCaption
          ParentColor = False
          TabOrder = 0
        end
      end
      object PageControl: TPageControl
        Left = 1
        Height = 209
        Top = 17
        Width = 346
        TabStop = False
        ActivePage = tabRoot
        Align = alClient
        Constraints.MinWidth = 118
        TabIndex = 0
        TabOrder = 1
        object tabRoot: TTabSheet
          Caption = 'tabRoot'
          ClientHeight = 183
          ClientWidth = 338
          OnShow = tabRootShow
          PopupMenu = popRootMenu
          TabVisible = False
          object lbDelimitersRoot: TLabel
            Left = 0
            Height = 14
            Top = 134
            Width = 46
            Caption = '&Delimiters'
            FocusControl = edDelimitersRoot
            ParentColor = False
          end
          object Label3: TLabel
            Left = 0
            Height = 14
            Top = 80
            Width = 71
            Caption = 'File with styles:'
            Enabled = False
            ParentColor = False
          end
          object Label5: TLabel
            Left = 0
            Height = 14
            Top = 104
            Width = 68
            Caption = 'Color scheme:'
            Enabled = False
            ParentColor = False
          end
          object chCaseRoot: TCheckBox
            Left = 0
            Height = 17
            Top = 0
            Width = 90
            Caption = '&Case Sensitive'
            OnClick = RootChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 0
          end
          object chEnabledRoot: TCheckBox
            Left = 269
            Height = 17
            Top = 0
            Width = 59
            Anchors = [akTop, akRight]
            Caption = '&Enabled'
            OnClick = RootChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 1
          end
          object edDelimitersRoot: TEdit
            Left = 70
            Height = 21
            Top = 131
            Width = 259
            Anchors = [akTop, akLeft, akRight]
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            OnChange = RootChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            TabOrder = 2
          end
          object pRootButtons: TPanel
            Left = 0
            Height = 24
            Top = 159
            Width = 338
            Align = alBottom
            BevelOuter = bvNone
            ClientHeight = 24
            ClientWidth = 338
            TabOrder = 3
            object btAddRangeRoot: TButton
              Left = 0
              Height = 24
              Top = 0
              Width = 108
              Caption = 'Add &Range'
              OnClick = DoAddRangeToRoot
              TabOrder = 0
            end
            object btAddKeywordsRoot: TButton
              Left = 110
              Height = 24
              Top = 0
              Width = 109
              Caption = 'Add &Keywords'
              OnClick = DoAddKeywordToRoot
              TabOrder = 1
            end
            object btAddSetRoot: TButton
              Left = 221
              Height = 24
              Top = 0
              Width = 108
              Caption = 'Add &Set'
              OnClick = DoAddSetToRoot
              TabOrder = 2
            end
          end
          object edStylesFile: TEdit
            Left = 70
            Height = 21
            Top = 76
            Width = 236
            Anchors = [akTop, akLeft, akRight]
            Color = clBtnFace
            Enabled = False
            OnChange = RootChange
            PopupMenu = popStandard
            TabOrder = 4
            Text = 'Standart.clr'
          end
          object btStylesFile: TButton
            Left = 310
            Height = 20
            Top = 76
            Width = 20
            Anchors = [akTop, akRight]
            Caption = '...'
            Enabled = False
            OnClick = btStylesFileClick
            TabOrder = 5
            TabStop = False
          end
          object ComboBox2: TComboBox
            Left = 70
            Height = 21
            Top = 100
            Width = 262
            Anchors = [akTop, akLeft, akRight]
            Color = clBtnFace
            Enabled = False
            ItemHeight = 13
            ItemIndex = 0
            Items.Strings = (
              'Standard'
              'Twinight'
              'FAR'
              'Visual Studio'
            )
            Style = csDropDownList
            TabOrder = 6
            Text = 'Standard'
          end
          object Button4: TButton
            Left = 232
            Height = 25
            Top = 48
            Width = 97
            Caption = 'Add link to range'
            OnClick = DoAddRangeLink
            TabOrder = 7
            Visible = False
          end
        end
        object tabRange: TTabSheet
          Caption = 'tabRange'
          ClientHeight = 183
          ClientWidth = 338
          ImageIndex = 1
          OnShow = tabRangeShow
          PopupMenu = popRangeMenu
          TabVisible = False
          object lbDelimitersRange: TLabel
            Left = 0
            Height = 14
            Top = 134
            Width = 46
            Caption = '&Delimiters'
            FocusControl = edDelimitersRange
            ParentColor = False
          end
          object lbRangeFrom: TLabel
            Left = 0
            Height = 14
            Top = 20
            Width = 27
            Caption = '&From:'
            ParentColor = False
          end
          object lbRangeTo: TLabel
            Left = 0
            Height = 14
            Top = 42
            Width = 17
            Caption = '&To:'
            ParentColor = False
          end
          object edDelimitersRange: TEdit
            Left = 70
            Height = 21
            Top = 131
            Width = 259
            Anchors = [akTop, akLeft, akRight]
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            OnChange = RangeChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            TabOrder = 14
          end
          object chEnabledRange: TCheckBox
            Left = 269
            Height = 17
            Top = 0
            Width = 59
            Anchors = [akTop, akRight]
            Caption = '&Enabled'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 2
          end
          object chCaseRange: TCheckBox
            Left = 30
            Height = 17
            Top = 0
            Width = 90
            Caption = '&Case Sensitive'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 1
          end
          object pRangeButtons: TPanel
            Left = 0
            Height = 24
            Top = 159
            Width = 338
            Align = alBottom
            BevelOuter = bvNone
            ClientHeight = 24
            ClientWidth = 338
            TabOrder = 15
            object btAddRange: TButton
              Left = 0
              Height = 24
              Top = 0
              Width = 108
              Caption = 'Add &Range'
              OnClick = DoAddRange
              TabOrder = 0
            end
            object btAddKeywords: TButton
              Left = 110
              Height = 24
              Top = 0
              Width = 109
              Caption = 'Add &Keywords'
              OnClick = DoAddKeyword
              TabOrder = 1
            end
            object btAddSet: TButton
              Left = 221
              Height = 24
              Top = 0
              Width = 108
              Caption = 'Add &Set'
              OnClick = DoAddSet
              TabOrder = 2
            end
          end
          object btChooseRule: TButton
            Left = 0
            Height = 16
            Top = 0
            Width = 27
            Caption = '7'
            Enabled = False
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Marlett'
            OnClick = btTagMenuClick
            ParentFont = False
            TabOrder = 0
          end
          object edFrom: TEdit
            Left = 30
            Height = 21
            Top = 18
            Width = 226
            Anchors = [akTop, akLeft, akRight]
            AutoSelect = False
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            OnChange = RangeChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            TabOrder = 3
          end
          object edTo: TEdit
            Left = 30
            Height = 21
            Top = 40
            Width = 226
            Anchors = [akTop, akLeft, akRight]
            AutoSelect = False
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            OnChange = RangeChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            TabOrder = 4
          end
          object btFromList: TButton
            Left = 259
            Height = 19
            Top = 18
            Width = 19
            Anchors = [akTop, akRight]
            Caption = '...'
            Enabled = False
            TabOrder = 5
            TabStop = False
          end
          object chFromEOL: TCheckBox
            Left = 302
            Height = 17
            Top = 19
            Width = 26
            Anchors = [akTop, akRight]
            Caption = '¶'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 9
          end
          object btToList: TButton
            Left = 259
            Height = 19
            Top = 40
            Width = 19
            Anchors = [akTop, akRight]
            Caption = '...'
            Enabled = False
            TabOrder = 6
            TabStop = False
          end
          object btToMenu: TButton
            Left = 281
            Height = 19
            Top = 40
            Width = 19
            Anchors = [akTop, akRight]
            Caption = 'u'
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Marlett'
            OnClick = btTagMenuClick
            ParentFont = False
            PopupMenu = popCloseTagMenu
            TabOrder = 8
          end
          object btFromMenu: TButton
            Left = 281
            Height = 19
            Top = 18
            Width = 19
            Anchors = [akTop, akRight]
            Caption = 'u'
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Marlett'
            OnClick = btTagMenuClick
            ParentFont = False
            PopupMenu = popOpenTagMenu
            TabOrder = 7
          end
          object chToEOL: TCheckBox
            Left = 302
            Height = 17
            Top = 41
            Width = 26
            Anchors = [akTop, akRight]
            Caption = '¶'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 10
          end
          object chCloseOnWord: TCheckBox
            Left = 0
            Height = 17
            Top = 61
            Width = 102
            Caption = 'Close on &delimiter'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 11
          end
          object chCloseOnEOL: TCheckBox
            Left = 0
            Height = 17
            Top = 77
            Width = 113
            Caption = 'Close on end of &line'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 12
          end
          object chCloseParent: TCheckBox
            Left = 0
            Height = 17
            Top = 93
            Width = 161
            Caption = 'Close &parent if same close tag'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 13
          end
          object CheckBox8: TCheckBox
            Left = 0
            Height = 17
            Top = 109
            Width = 161
            Caption = 'Close &parent if same close tag'
            OnClick = RangeChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 16
            Visible = False
          end
          object Button3: TButton
            Left = 232
            Height = 25
            Top = 64
            Width = 97
            Caption = 'Add link to range'
            OnClick = DoAddRangeLink
            TabOrder = 17
            Visible = False
          end
        end
        object tabKeywords: TTabSheet
          Caption = 'tabKeywords'
          ClientHeight = 183
          ClientWidth = 338
          ImageIndex = 2
          OnShow = tabKeywordsShow
          PopupMenu = popKeywordsMenu
          TabVisible = False
          object pProp: TPanel
            Left = 264
            Height = 183
            Top = 0
            Width = 74
            Align = alRight
            BevelOuter = bvNone
            ClientHeight = 183
            ClientWidth = 74
            Constraints.MinWidth = 70
            TabOrder = 1
            object lbKeywordCount: TLabel
              Left = 3
              Height = 14
              Top = 168
              Width = 38
              Anchors = [akLeft, akBottom]
              Caption = 'Lines: 0'
              ParentColor = False
            end
            object btSort: TSpeedButton
              Left = 3
              Height = 22
              Hint = 'Sort strings'
              Top = 24
              Width = 23
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                04000000000080000000C40E0000C40E00001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                DDDDDDCCCCDDDD0DDDDDDDCDDDDDD707DDDDDDDCCDDDD000DDDDDDDDDCDD7000
                7DDDDDCDDCDD00000DDDDDDCCDDDDD0DDDDDDDDDDDDDDD0DDDDDDDDD9DDDDD0D
                DDDDDDDD9DDDDD0DDDDDDDDD9DDDDD0DDDDDDD9D9DDDDD0DDDDDDDD99DDDDD0D
                DDDDDDDD9DDDDD0DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
              }
              NumGlyphs = 0
              OnClick = btSort_oldClick
              ShowHint = True
              ParentShowHint = False
            end
            object btLowerCase: TSpeedButton
              Left = 27
              Height = 22
              Hint = 'Lower case'
              Top = 24
              Width = 23
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
                DDDD0DDDDD0DDD0DDD0D0DDDDD00DD0DDD0DD00000D00DD000DDD0DDD0D000D0
                D0DDDD0D0DD00DDD0DDDDD0D0DD0DDDD0DDDDDD0DDDDDDDDDDDDDDD0DDDDDDDD
                DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
              }
              NumGlyphs = 0
              OnClick = btLowerCase_oldClick
              ShowHint = True
              ParentShowHint = False
            end
            object btSpacesToEol: TSpeedButton
              Left = 51
              Height = 22
              Hint = 'Spaces to EOL'
              Top = 24
              Width = 23
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
                DDDDDDDDDDDDDDDDDDDDDDDDDD0DD0DDDDDDDDDDDD0DD0DDDDDDDDDDDD0DD0DD
                DDDDDDDDDD0DD0DDDDDDDDDDDD0DD0DDDDDDDDDD000DD0DDDDDDDDD0000DD0DD
                DDDDDD00000DD0DDDDDDDD00000DD0DDDDDDDD00000DD0DDDDDDDDD0000DD0DD
                DDDDDDDD00000000DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
              }
              NumGlyphs = 0
              OnClick = btSpacesToEol_oldClick
              ShowHint = True
              ParentShowHint = False
            end
            object chEnabledKeyList: TCheckBox
              Left = 13
              Height = 17
              Top = 0
              Width = 59
              Anchors = [akTop, akRight]
              Caption = '&Enabled'
              OnClick = KeywordsChange
              OnContextPopup = DontNeedContextPopup
              OnMouseDown = CheckBoxMouseDown
              TabOrder = 0
            end
            object btSort_old: TButton
              Left = 3
              Height = 23
              Top = 84
              Width = 67
              Caption = '&Sort'
              OnClick = btSort_oldClick
              TabOrder = 1
              Visible = False
            end
            object btLowerCase_old: TButton
              Left = 3
              Height = 23
              Top = 108
              Width = 67
              Caption = '&Lower Case'
              OnClick = btLowerCase_oldClick
              TabOrder = 2
              Visible = False
            end
            object btSpacesToEol_old: TButton
              Left = 3
              Height = 23
              Top = 132
              Width = 67
              Caption = 'S&paces -> ¶'
              OnClick = btSpacesToEol_oldClick
              TabOrder = 3
              Visible = False
            end
          end
          object Memo: TMemo
            Left = 0
            Height = 183
            Top = 0
            Width = 264
            Align = alClient
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            Lines.Strings = (
              'Memo'
            )
            OnChange = KeywordsChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
        object tabSet: TTabSheet
          Caption = 'tabSet'
          ClientHeight = 183
          ClientWidth = 338
          ImageIndex = 3
          OnShow = tabSetShow
          PopupMenu = popSetMenu
          TabVisible = False
          object lbSymbSet: TLabel
            Left = 0
            Height = 14
            Top = 22
            Width = 57
            Caption = '&Symbol Set:'
            FocusControl = edSymbSet
            ParentColor = False
          end
          object chEnabledSet: TCheckBox
            Left = 269
            Height = 17
            Top = 0
            Width = 59
            Anchors = [akTop, akRight]
            Caption = '&Enabled'
            OnClick = SetChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 1
          end
          object edSymbSet: TEdit
            Left = 70
            Height = 21
            Top = 19
            Width = 259
            Anchors = [akTop, akLeft, akRight]
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            OnChange = SetChange
            OnContextPopup = EditContextPopup
            OnKeyDown = EditKeyDown
            ParentFont = False
            PopupMenu = popStandard
            TabOrder = 2
          end
          object chAnyStart: TCheckBox
            Left = 0
            Height = 17
            Top = 0
            Width = 60
            Caption = 'AnyStart'
            Enabled = False
            OnClick = SetChange
            OnContextPopup = DontNeedContextPopup
            OnMouseDown = CheckBoxMouseDown
            TabOrder = 0
            TabStop = False
          end
        end
        object tabSeveralRules: TTabSheet
          Caption = 'tabSeveralRules'
          ClientHeight = 183
          ClientWidth = 338
          ImageIndex = 4
          TabVisible = False
          object Label1: TLabel
            Left = 4
            Height = 15
            Top = 2
            Width = 321
            Alignment = taCenter
            Anchors = [akTop, akLeft, akRight]
            AutoSize = False
            Caption = 'You have selected several rules...'
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
          end
        end
      end
    end
  end
  object pBottom: TPanel
    Left = 0
    Height = 185
    Top = 232
    Width = 640
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvLowered
    ClientHeight = 185
    ClientWidth = 640
    Constraints.MinHeight = 17
    TabOrder = 2
    object Bevel5: TBevel
      Left = 2
      Height = 1
      Top = 17
      Width = 636
      Align = alTop
      Style = bsRaised
    end
    object pBottomParentCapt: TPanel
      Left = 2
      Height = 15
      Top = 2
      Width = 636
      Align = alTop
      BevelOuter = bvNone
      ClientHeight = 15
      ClientWidth = 636
      Color = clActiveCaption
      Font.Color = clCaptionText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      PopupMenu = popPanels
      TabOrder = 0
      object lbSampMin: TLabel
        Left = 602
        Height = 15
        Hint = 'Minimize'
        Top = 0
        Width = 17
        Align = alRight
        Caption = '0'
        Font.Color = clBtnFace
        Font.Height = -15
        Font.Name = 'Marlett'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lbSampMinClick
        OnMouseEnter = LabelMouseEnter
        OnMouseLeave = LabelMouseLeave
        OnContextPopup = LabelContextPopup
      end
      object lbSampMax: TLabel
        Left = 619
        Height = 15
        Hint = 'Maximize'
        Top = 0
        Width = 17
        Align = alRight
        Caption = '1'
        Font.Color = clBtnFace
        Font.Height = -15
        Font.Name = 'Marlett'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lbSampMaxClick
        OnMouseEnter = LabelMouseEnter
        OnMouseLeave = LabelMouseLeave
        OnContextPopup = LabelContextPopup
      end
      object Label6: TLabel
        Left = 0
        Height = 15
        Hint = 'Minimize'
        Top = 0
        Width = 14
        Align = alLeft
        Caption = '<'
        Enabled = False
        Font.Color = clBtnFace
        Font.Height = -15
        Font.Name = 'Wingdings'
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lbSampMinClick
        OnMouseEnter = LabelMouseEnter
        OnMouseLeave = LabelMouseLeave
        OnContextPopup = LabelContextPopup
      end
      object pBottomCapt: TPanel
        Left = 14
        Height = 15
        Top = 0
        Width = 588
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Sample text (will not save)'
        Color = clActiveCaption
        ParentColor = False
        TabOrder = 0
        OnDblClick = PanelDblClick
      end
    end
    inline SampleMemo: TSynEdit
      Left = 2
      Height = 165
      Top = 18
      Width = 636
      Align = alClient
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Quality = fqNonAntialiased
      ParentColor = False
      ParentFont = False
      PopupMenu = popSampleMemoMenu
      TabOrder = 1
      OnKeyDown = SampleMemoKeyDown
      OnMouseDown = SampleMemoMouseDown
      Gutter.Width = 57
      Gutter.MouseActions = <      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccAny
          ClickDir = cdDown
          Command = 13
          MoveCaret = False
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbRight
          ClickCount = ccSingle
          ClickDir = cdUp
          Command = 12
          MoveCaret = False
          Option = 0
          Priority = 0
        end>
      RightGutter.Width = 0
      RightGutter.MouseActions = <      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccAny
          ClickDir = cdDown
          Command = 13
          MoveCaret = False
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbRight
          ClickCount = ccSingle
          ClickDir = cdUp
          Command = 12
          MoveCaret = False
          Option = 0
          Priority = 0
        end>
      Highlighter = SynUniSyn
      Keystrokes = <      
        item
          Command = ecUp
          ShortCut = 38
        end      
        item
          Command = ecSelUp
          ShortCut = 8230
        end      
        item
          Command = ecScrollUp
          ShortCut = 16422
        end      
        item
          Command = ecDown
          ShortCut = 40
        end      
        item
          Command = ecSelDown
          ShortCut = 8232
        end      
        item
          Command = ecScrollDown
          ShortCut = 16424
        end      
        item
          Command = ecLeft
          ShortCut = 37
        end      
        item
          Command = ecSelLeft
          ShortCut = 8229
        end      
        item
          Command = ecWordLeft
          ShortCut = 16421
        end      
        item
          Command = ecSelWordLeft
          ShortCut = 24613
        end      
        item
          Command = ecRight
          ShortCut = 39
        end      
        item
          Command = ecSelRight
          ShortCut = 8231
        end      
        item
          Command = ecWordRight
          ShortCut = 16423
        end      
        item
          Command = ecSelWordRight
          ShortCut = 24615
        end      
        item
          Command = ecPageDown
          ShortCut = 34
        end      
        item
          Command = ecSelPageDown
          ShortCut = 8226
        end      
        item
          Command = ecPageBottom
          ShortCut = 16418
        end      
        item
          Command = ecSelPageBottom
          ShortCut = 24610
        end      
        item
          Command = ecPageUp
          ShortCut = 33
        end      
        item
          Command = ecSelPageUp
          ShortCut = 8225
        end      
        item
          Command = ecPageTop
          ShortCut = 16417
        end      
        item
          Command = ecSelPageTop
          ShortCut = 24609
        end      
        item
          Command = ecLineStart
          ShortCut = 36
        end      
        item
          Command = ecSelLineStart
          ShortCut = 8228
        end      
        item
          Command = ecEditorTop
          ShortCut = 16420
        end      
        item
          Command = ecSelEditorTop
          ShortCut = 24612
        end      
        item
          Command = ecLineEnd
          ShortCut = 35
        end      
        item
          Command = ecSelLineEnd
          ShortCut = 8227
        end      
        item
          Command = ecEditorBottom
          ShortCut = 16419
        end      
        item
          Command = ecSelEditorBottom
          ShortCut = 24611
        end      
        item
          Command = ecToggleMode
          ShortCut = 45
        end      
        item
          Command = ecCopy
          ShortCut = 16429
        end      
        item
          Command = ecPaste
          ShortCut = 8237
        end      
        item
          Command = ecDeleteChar
          ShortCut = 46
        end      
        item
          Command = ecCut
          ShortCut = 8238
        end      
        item
          Command = ecDeleteLastChar
          ShortCut = 8
        end      
        item
          Command = ecDeleteLastChar
          ShortCut = 8200
        end      
        item
          Command = ecDeleteLastWord
          ShortCut = 16392
        end      
        item
          Command = ecUndo
          ShortCut = 32776
        end      
        item
          Command = ecRedo
          ShortCut = 40968
        end      
        item
          Command = ecLineBreak
          ShortCut = 13
        end      
        item
          Command = ecSelectAll
          ShortCut = 16449
        end      
        item
          Command = ecCopy
          ShortCut = 16451
        end      
        item
          Command = ecBlockIndent
          ShortCut = 24649
        end      
        item
          Command = ecLineBreak
          ShortCut = 16461
        end      
        item
          Command = ecInsertLine
          ShortCut = 16462
        end      
        item
          Command = ecDeleteWord
          ShortCut = 16468
        end      
        item
          Command = ecBlockUnindent
          ShortCut = 24661
        end      
        item
          Command = ecPaste
          ShortCut = 16470
        end      
        item
          Command = ecCut
          ShortCut = 16472
        end      
        item
          Command = ecDeleteLine
          ShortCut = 16473
        end      
        item
          Command = ecDeleteEOL
          ShortCut = 24665
        end      
        item
          Command = ecUndo
          ShortCut = 16474
        end      
        item
          Command = ecRedo
          ShortCut = 24666
        end      
        item
          Command = ecGotoMarker0
          ShortCut = 16432
        end      
        item
          Command = ecGotoMarker1
          ShortCut = 16433
        end      
        item
          Command = ecGotoMarker2
          ShortCut = 16434
        end      
        item
          Command = ecGotoMarker3
          ShortCut = 16435
        end      
        item
          Command = ecGotoMarker4
          ShortCut = 16436
        end      
        item
          Command = ecGotoMarker5
          ShortCut = 16437
        end      
        item
          Command = ecGotoMarker6
          ShortCut = 16438
        end      
        item
          Command = ecGotoMarker7
          ShortCut = 16439
        end      
        item
          Command = ecGotoMarker8
          ShortCut = 16440
        end      
        item
          Command = ecGotoMarker9
          ShortCut = 16441
        end      
        item
          Command = ecSetMarker0
          ShortCut = 24624
        end      
        item
          Command = ecSetMarker1
          ShortCut = 24625
        end      
        item
          Command = ecSetMarker2
          ShortCut = 24626
        end      
        item
          Command = ecSetMarker3
          ShortCut = 24627
        end      
        item
          Command = ecSetMarker4
          ShortCut = 24628
        end      
        item
          Command = ecSetMarker5
          ShortCut = 24629
        end      
        item
          Command = ecSetMarker6
          ShortCut = 24630
        end      
        item
          Command = ecSetMarker7
          ShortCut = 24631
        end      
        item
          Command = ecSetMarker8
          ShortCut = 24632
        end      
        item
          Command = ecSetMarker9
          ShortCut = 24633
        end      
        item
          Command = EcFoldLevel1
          ShortCut = 41009
        end      
        item
          Command = EcFoldLevel2
          ShortCut = 41010
        end      
        item
          Command = EcFoldLevel1
          ShortCut = 41011
        end      
        item
          Command = EcFoldLevel1
          ShortCut = 41012
        end      
        item
          Command = EcFoldLevel1
          ShortCut = 41013
        end      
        item
          Command = EcFoldLevel6
          ShortCut = 41014
        end      
        item
          Command = EcFoldLevel7
          ShortCut = 41015
        end      
        item
          Command = EcFoldLevel8
          ShortCut = 41016
        end      
        item
          Command = EcFoldLevel9
          ShortCut = 41017
        end      
        item
          Command = EcFoldLevel0
          ShortCut = 41008
        end      
        item
          Command = EcFoldCurrent
          ShortCut = 41005
        end      
        item
          Command = EcUnFoldCurrent
          ShortCut = 41003
        end      
        item
          Command = EcToggleMarkupWord
          ShortCut = 32845
        end      
        item
          Command = ecNormalSelect
          ShortCut = 24654
        end      
        item
          Command = ecColumnSelect
          ShortCut = 24643
        end      
        item
          Command = ecLineSelect
          ShortCut = 24652
        end      
        item
          Command = ecTab
          ShortCut = 9
        end      
        item
          Command = ecShiftTab
          ShortCut = 8201
        end      
        item
          Command = ecMatchBracket
          ShortCut = 24642
        end      
        item
          Command = ecColSelUp
          ShortCut = 40998
        end      
        item
          Command = ecColSelDown
          ShortCut = 41000
        end      
        item
          Command = ecColSelLeft
          ShortCut = 40997
        end      
        item
          Command = ecColSelRight
          ShortCut = 40999
        end      
        item
          Command = ecColSelPageDown
          ShortCut = 40994
        end      
        item
          Command = ecColSelPageBottom
          ShortCut = 57378
        end      
        item
          Command = ecColSelPageUp
          ShortCut = 40993
        end      
        item
          Command = ecColSelPageTop
          ShortCut = 57377
        end      
        item
          Command = ecColSelLineStart
          ShortCut = 40996
        end      
        item
          Command = ecColSelLineEnd
          ShortCut = 40995
        end      
        item
          Command = ecColSelEditorTop
          ShortCut = 57380
        end      
        item
          Command = ecColSelEditorBottom
          ShortCut = 57379
        end>
      MouseActions = <      
        item
          Shift = []
          ShiftMask = [ssShift, ssAlt]
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 1
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = [ssShift]
          ShiftMask = [ssShift, ssAlt]
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 1
          MoveCaret = True
          Option = 1
          Priority = 0
        end      
        item
          Shift = [ssAlt]
          ShiftMask = [ssShift, ssAlt]
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 3
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = [ssShift, ssAlt]
          ShiftMask = [ssShift, ssAlt]
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 3
          MoveCaret = True
          Option = 1
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbRight
          ClickCount = ccSingle
          ClickDir = cdUp
          Command = 12
          MoveCaret = False
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccDouble
          ClickDir = cdDown
          Command = 6
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccTriple
          ClickDir = cdDown
          Command = 7
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccQuad
          ClickDir = cdDown
          Command = 8
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = []
          ShiftMask = []
          Button = mbMiddle
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 10
          MoveCaret = True
          Option = 0
          Priority = 0
        end      
        item
          Shift = [ssCtrl]
          ShiftMask = [ssShift, ssAlt, ssCtrl]
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdUp
          Command = 11
          MoveCaret = False
          Option = 0
          Priority = 0
        end>
      MouseSelActions = <      
        item
          Shift = []
          ShiftMask = []
          Button = mbLeft
          ClickCount = ccSingle
          ClickDir = cdDown
          Command = 9
          MoveCaret = False
          Option = 0
          Priority = 0
        end>
      Lines.Strings = (
        'SynEdit1'
      )
      BracketHighlightStyle = sbhsBoth
      inline SynLeftGutterPartList1: TSynGutterPartList
        object SynGutterMarks1: TSynGutterMarks
          Width = 24
        end
        object SynGutterLineNumber1: TSynGutterLineNumber
          Width = 17
          MouseActions = <>
          MarkupInfo.Background = clBtnFace
          MarkupInfo.Foreground = clNone
          DigitCount = 2
          ShowOnlyLineNumbersMultiplesOf = 1
          ZeroStart = False
          LeadingZeros = False
        end
        object SynGutterChanges1: TSynGutterChanges
          Width = 4
          ModifiedColor = 59900
          SavedColor = clGreen
        end
        object SynGutterSeparator1: TSynGutterSeparator
          Width = 2
        end
        object SynGutterCodeFolding1: TSynGutterCodeFolding
          MouseActions = <          
            item
              Shift = []
              ShiftMask = []
              Button = mbRight
              ClickCount = ccSingle
              ClickDir = cdUp
              Command = 16
              MoveCaret = False
              Option = 0
              Priority = 0
            end          
            item
              Shift = []
              ShiftMask = [ssShift]
              Button = mbMiddle
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 14
              MoveCaret = False
              Option = 0
              Priority = 0
            end          
            item
              Shift = [ssShift]
              ShiftMask = [ssShift]
              Button = mbMiddle
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 14
              MoveCaret = False
              Option = 1
              Priority = 0
            end          
            item
              Shift = []
              ShiftMask = []
              Button = mbLeft
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 0
              MoveCaret = False
              Option = 0
              Priority = 0
            end>
          MarkupInfo.Background = clNone
          MarkupInfo.Foreground = clGray
          MouseActionsExpanded = <          
            item
              Shift = []
              ShiftMask = []
              Button = mbLeft
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 14
              MoveCaret = False
              Option = 0
              Priority = 0
            end>
          MouseActionsCollapsed = <          
            item
              Shift = [ssCtrl]
              ShiftMask = [ssCtrl]
              Button = mbLeft
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 15
              MoveCaret = False
              Option = 0
              Priority = 0
            end          
            item
              Shift = []
              ShiftMask = [ssCtrl]
              Button = mbLeft
              ClickCount = ccAny
              ClickDir = cdDown
              Command = 15
              MoveCaret = False
              Option = 1
              Priority = 0
            end>
        end
      end
    end
  end
  object pButtons: TPanel
    Left = 0
    Height = 35
    Top = 420
    Width = 640
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvLowered
    ClientHeight = 35
    ClientWidth = 640
    Constraints.MinHeight = 35
    TabOrder = 3
    object btOk: TButton
      Left = 400
      Height = 25
      Top = 5
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      OnClick = btOkClick
      TabOrder = 0
    end
    object btCancel: TButton
      Left = 480
      Height = 25
      Top = 5
      Width = 75
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      OnClick = btCancelClick
      TabOrder = 1
    end
    object btApply: TButton
      Left = 560
      Height = 25
      Top = 5
      Width = 75
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      Enabled = False
      OnClick = btApplyClick
      TabOrder = 2
    end
  end
  object popStandard: TPopupMenu
    Images = listImages
    left = 16
    top = 32
    object popUndo: TMenuItem
      Caption = '&Undo'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF000000FF000000FF000000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 7
      ShortCut = 16474
      OnClick = popUndoClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object popCut: TMenuItem
      Caption = 'Cu&t'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = popCutClick
    end
    object popCopy: TMenuItem
      Caption = '&Copy'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = popCopyClick
    end
    object popPaste: TMenuItem
      Caption = '&Paste'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = popPasteClick
    end
    object popDelete: TMenuItem
      Caption = '&Delete'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 8
      ShortCut = 46
      OnClick = popDeleteClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object popSelectAll: TMenuItem
      Caption = 'Select &All'
      ShortCut = 16449
      OnClick = popSelectAllClick
    end
  end
  object popOpenTagMenu: TPopupMenu
    left = 48
    top = 32
    object Closemenu1: TMenuItem
      Caption = 'Close menu'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Opentagisfirstsymbolsonline1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is first symbols on line'
      OnClick = miOpenTagMenuClick
    end
    object Opentagisfirstnonspacesymbolsonline1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is first non-space symbols on line'
      OnClick = miOpenTagMenuClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Opentagispartofterm1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is part of term'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object Opentagispartoftermonlyrightside1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is part of term (only right side)'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object Opentagispartoftermonlyleftside1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is part of term (only left side)'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object Opentagisnotpartofterm1: TMenuItem
      AutoCheck = True
      Caption = 'Open tag is not part of term'
      RadioItem = True
      OnClick = miTagMenuClick
    end
  end
  object popCloseTagMenu: TPopupMenu
    left = 80
    top = 32
    object MenuItem1: TMenuItem
      Caption = 'Close menu'
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is first symbols on line'
      OnClick = miCloseTagMenuClick
    end
    object MenuItem4: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is first non-space symbols on line'
      OnClick = miCloseTagMenuClick
    end
    object MenuItem5: TMenuItem
      Caption = '-'
    end
    object MenuItem6: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is part of term'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object MenuItem7: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is part of term (only right side)'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object MenuItem8: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is part of term (only left side)'
      RadioItem = True
      OnClick = miTagMenuClick
    end
    object MenuItem9: TMenuItem
      AutoCheck = True
      Caption = 'Close tag is not part of term'
      RadioItem = True
      OnClick = miTagMenuClick
    end
  end
  object popRootMenu: TPopupMenu
    Images = listImages
    left = 16
    top = 64
    object rootCut: TMenuItem
      Caption = 'Cu&t Root range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = rootCutClick
    end
    object rootCopy: TMenuItem
      Caption = '&Copy Root range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = rootCopyClick
    end
    object rootPaste: TMenuItem
      Caption = '&Paste inside Root'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = rootPasteInsideClick
    end
    object rootPasteAndReplace: TMenuItem
      Caption = 'Paste and Re&place Root'
      OnClick = rootPasteAndReplaceClick
    end
    object rootBreak1: TMenuItem
      Caption = '-'
    end
    object rootLoadFromFile: TMenuItem
      Caption = '&Load Root from File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFF000000FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 5
      OnClick = rootLoadFromFileClick
    end
    object rootSaveToFile: TMenuItem
      Caption = 'Save Highlighter to File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 6
      OnClick = rootSaveToFileClick
    end
    object rootBreak2: TMenuItem
      Caption = '-'
    end
    object rootAddRange: TMenuItem
      Caption = 'Add &Range to Root'
      OnClick = DoAddRangeToRoot
    end
    object rootAddKeywords: TMenuItem
      Caption = 'Add &Keywords to Root'
      OnClick = DoAddKeywordToRoot
    end
    object rootAddSetto: TMenuItem
      Caption = 'Add &Set to Root'
      OnClick = DoAddSetToRoot
    end
    object rootBreak3: TMenuItem
      Caption = '-'
    end
    object rootRename: TMenuItem
      Caption = 'Re&name Root range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 3
      ShortCut = 113
      OnClick = DoRenameNode
    end
    object rootDeleteAll: TMenuItem
      Caption = '&Delete all rules'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 4
      ShortCut = 46
      OnClick = DoDeleteNode
    end
    object rootBreak4: TMenuItem
      Caption = '-'
    end
    object rootInfo: TMenuItem
      Caption = 'Highlighter Info...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF
        FFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FFC0C0C0FF0000
        00FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FFC0C0C0FF000000FFFFFFFFFF000000FFC0C0C0FF000000FFC0C0
        C0FF000000FF000000FF000000FF000000FF800000FF800000FF000000FFFFFF
        FFFFFFFFFFFF000000FFC0C0C0FF000000FFC0C0C0FF000000FFC0C0C0FF0000
        00FFC0C0C0FFC0C0C0FFC0C0C0FF000000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FFC0C0C0FF000000FFC0C0C0FF000000FFC0C0
        C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FFC0C0C0FF000000FFC0C0C0FFC0C0
        C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FFC0C0C0FFC0C0C0FFC0C0
        C0FFC0C0C0FFC0C0C0FFC0C0C0FF000000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 11
      ShortCut = 16496
      OnClick = rootInfoClick
    end
  end
  object popRangeMenu: TPopupMenu
    Images = listImages
    left = 48
    top = 64
    object rangeBack: TMenuItem
      Caption = '&Up to one level'
      OnClick = lbPropBackClick
    end
    object rangeBreak1: TMenuItem
      Caption = '-'
    end
    object rangeCut: TMenuItem
      Caption = 'Cu&t Range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = rangeCutClick
    end
    object rangeCopy: TMenuItem
      Caption = '&Copy Range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = rangeCopyClick
    end
    object rangePaste: TMenuItem
      Caption = '&Paste inside Range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = rangePasteInsideClick
    end
    object rangePasteAndReplace: TMenuItem
      Caption = 'Paste and Re&place'
      OnClick = rangePasteAndReplaceClick
    end
    object rangePasteNextTo: TMenuItem
      Caption = 'Paste next to'
      OnClick = rangePasteNextToClick
    end
    object rangeBreak2: TMenuItem
      Caption = '-'
    end
    object rangeLoadFromFile: TMenuItem
      Caption = '&Load from File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFF000000FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 5
      OnClick = rangeLoadFromFileClick
    end
    object rangeSaveToFile: TMenuItem
      Caption = 'Save Range to File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 6
      OnClick = rangeSaveToFileClick
    end
    object rangeBreak3: TMenuItem
      Caption = '-'
    end
    object rangeAddRange: TMenuItem
      Caption = 'Add &Range'
      OnClick = DoAddRange
    end
    object rangeAddKeywords: TMenuItem
      Caption = 'Add &Keywords'
      OnClick = DoAddKeyword
    end
    object rangeAddSet: TMenuItem
      Caption = 'Add &Set'
      OnClick = DoAddSet
    end
    object rangeBreak4: TMenuItem
      Caption = '-'
    end
    object rangeRename: TMenuItem
      Caption = 'Re&name Range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 3
      ShortCut = 113
      OnClick = DoRenameNode
    end
    object rangeDelete: TMenuItem
      Caption = '&Delete Range'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 4
      ShortCut = 46
      OnClick = DoDeleteNode
    end
  end
  object popKeywordsMenu: TPopupMenu
    Images = listImages
    left = 80
    top = 64
    object keywordsBack: TMenuItem
      Caption = '&Up to one level'
      OnClick = lbRuleMenuClick
    end
    object keywordsBreak1: TMenuItem
      Caption = '-'
    end
    object keywordsCut: TMenuItem
      Caption = 'Cu&t Keywords'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = rangeCutClick
    end
    object keywordsCopy: TMenuItem
      Caption = '&Copy Keywords'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = rangeCopyClick
    end
    object keywordsPaste: TMenuItem
      Caption = 'Paste next to'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = rangePasteNextToClick
    end
    object keywordsPasteAndReplace: TMenuItem
      Caption = 'Paste and Re&place'
    end
    object keywordsBreak2: TMenuItem
      Caption = '-'
    end
    object keywordsLoadFromFile: TMenuItem
      Caption = '&Load from File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFF000000FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 5
      OnClick = rangeLoadFromFileClick
    end
    object keywordsSaveToFile: TMenuItem
      Caption = 'Save Keywords to File ...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 6
      OnClick = rangeSaveToFileClick
    end
    object keywordsBreak3: TMenuItem
      Caption = '-'
    end
    object keywordsRename: TMenuItem
      Caption = 'Re&name Keywords'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 3
      ShortCut = 113
      OnClick = DoRenameNode
    end
    object keywordsDelete: TMenuItem
      Caption = '&Delete Keywords'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 4
      ShortCut = 46
      OnClick = DoDeleteNode
    end
  end
  object popSetMenu: TPopupMenu
    Images = listImages
    left = 112
    top = 64
    object setBack: TMenuItem
      Caption = '&Up to one level'
      OnClick = lbPropBackClick
    end
    object setBreak1: TMenuItem
      Caption = '-'
    end
    object setCut: TMenuItem
      Caption = 'Cu&t Set'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = rangeCutClick
    end
    object setCopy: TMenuItem
      Caption = '&Copy Set'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = rangeCopyClick
    end
    object setPaste: TMenuItem
      Caption = 'Paste next to'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = rangePasteNextToClick
    end
    object setPasteAndReplace: TMenuItem
      Caption = 'Paste and Re&place'
    end
    object setBreak2: TMenuItem
      Caption = '-'
    end
    object setLoadFromFile: TMenuItem
      Caption = '&Load from File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFF000000FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00FF
        FFFFFFFFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 5
      OnClick = rangeLoadFromFileClick
    end
    object setSaveToFile: TMenuItem
      Caption = 'Save Set to File...'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
        80FF008080FF008080FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF008080FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF008080FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF008080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 6
      OnClick = rangeSaveToFileClick
    end
    object setBreak3: TMenuItem
      Caption = '-'
    end
    object setRename: TMenuItem
      Caption = 'Re&name set'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FF000000FF000000FFFF0000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FFFF0000FF000000FFFF0000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FF000000FF000000FF000000FFFF0000FF000000FF000000FF0000
        00FF000000FF000000FF000000FFFF0000FF000000FF000000FF000000FF0000
        00FFFF0000FFFF0000FFFF0000FFFF0000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 3
      ShortCut = 113
      OnClick = DoRenameNode
    end
    object setDelete: TMenuItem
      Caption = '&Delete Set'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000
        FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000
        FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF0000
        00FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF0000FFFF0000FFFF000000FF000000FF000000FF000000FF0000
        FFFF0000FFFF0000FFFF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF0000FFFF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 4
      ShortCut = 46
      OnClick = DoDeleteNode
    end
  end
  object popPanels: TPopupMenu
    left = 112
    top = 32
    object RulesTree1: TMenuItem
      AutoCheck = True
      Caption = 'Panel "Rules Tree"'
      Checked = True
      OnClick = ShowHideTree
    end
    object Properties1: TMenuItem
      AutoCheck = True
      Caption = 'Panel "Properties"'
      Checked = True
      OnClick = ShowHideProp
    end
    object Attributes1: TMenuItem
      AutoCheck = True
      Caption = 'Panel "Attributes"'
      Checked = True
      OnClick = ShowHideAttr
    end
    object Sampletext1: TMenuItem
      AutoCheck = True
      Caption = 'Panel "Sample text"'
      Checked = True
      OnClick = ShowHideSamp
    end
    object Buttons1: TMenuItem
      AutoCheck = True
      Caption = 'Bottom Buttons'
      Checked = True
    end
  end
  object SynUniSyn: TSynUniSyn
    Enabled = False
    left = 80
    top = 160
  end
  object listImages: TImageList
    BkColor = clForeground
    DrawingStyle = dsTransparent
    left = 16
    top = 96
    Bitmap = {
      4C69160000001000000010000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFFF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFFF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFFF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FFFF00FF00000000FF000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FF000000FF800000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FFFF00FF00800000FF800000FF800000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008000
      00FF800000FF800000FFFF00FF00800000FFFF00FF00FF00FF00800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FFFF00
      FF00FF00FF00800000FFFF00FF00800000FFFF00FF00FF00FF00800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FFFF00
      FF00FF00FF00800000FFFF00FF00800000FFFF00FF00FF00FF00800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FFFF00
      FF00FF00FF00800000FFFF00FF00FF00FF00800000FF800000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008000
      00FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000FFFFFFFFFF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF000000FF000000FFFFFF
      FFFF000000FF800000FF800000FF800000FF800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF8000
      00FFFF00FF00FF00FF00FF00FF00000000FFFFFFFFFF000000FF000000FF0000
      00FF000000FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFF
      FFFF800000FFFF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFFFFFFFF000000FF000000FFFFFFFFFF800000FF8000
      00FF800000FF800000FFFF00FF00000000FFFFFFFFFF000000FF000000FF0000
      00FF000000FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFFFFFFFF000000FF000000FF000000FF000000FF0000
      00FFFFFFFFFF800000FFFF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FFFFFFFFFF000000FF000000FF000000FF000000FF0000
      00FFFFFFFFFF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF0000
      00FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00000000FF008080FF808080FF008080FF0000
      00FF00FFFFFF000000FF000000FF00FFFFFF000000FF808080FF008080FF8080
      80FF000000FFFF00FF00FF00FF00000000FF808080FF808080FF000000FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FF000000FF808080FF0080
      80FF000000FFFF00FF00FF00FF00000000FF008080FF808080FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF808080FF8080
      80FF000000FFFF00FF00FF00FF00000000FF808080FF008080FF808080FF0080
      80FF808080FF008080FF808080FF008080FF808080FF008080FF808080FF0080
      80FF000000FFFF00FF00FF00FF00000000FF008080FF808080FF008080FF8080
      80FF008080FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF000000FFFF00FF00FF00FF00000000FF808080FF008080FF808080FF0080
      80FF808080FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000
      00FF800000FFFF00FF00FF00FF00000000FF008080FF808080FF008080FF8080
      80FF008080FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000
      00FFFFFFFFFF800000FFFF00FF00000000FF808080FF008080FF808080FF0080
      80FF808080FF800000FFFFFFFFFF800000FF800000FF800000FFFFFFFFFF8000
      00FF800000FF800000FF800000FF000000FF008080FF808080FF008080FF8080
      80FF008080FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF800000FF000000FF808080FF008080FF808080FF0080
      80FF808080FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF8000
      00FF800000FFFFFFFFFF800000FFFF00FF00000000FF000000FF000000FF0000
      00FF000000FF800000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF0000FFFF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      00FFFF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      00FFFF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00FF00FF0000FFFF0000FFFF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      00FFFF0000FFFF0000FFFF0000FFFF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF0000FFFF0000FFFF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFFFF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFF0000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFF000000FFFF00FF00FF00FF00FF00FF00000000FF0000FFFF0000FFFF0000
      FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      FFFF0000FFFF0000FFFFFF00FF00FF00FF000000FFFF0000FFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF0000FFFF0000FFFF0000FFFF000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFF0000FFFF000000FF000000FF0000FFFF0000FFFFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFF0000FFFF000000FFFF00FF00FF00FF00000000FF0000FFFF0000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000
      FFFF000000FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF0000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFF0000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF0000FFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFFFF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF000000
      00FFFF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FFFF00FF00FF00FF00000000FF000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FF000000FF000000FFFF00FF00000000FF00FFFFFFFFFFFFFF00FFFFFF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF00FFFFFFFFFFFFFF00FF
      FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FF00FFFFFFFFFFFFFF00FFFFFFFFFF
      FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF00FFFFFFFFFFFFFF00FF
      FFFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF00FFFFFFFFFFFFFF00FFFFFF0000
      00FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
      80FF008080FF000000FFFF00FF00000000FFFFFFFFFF00FFFFFF000000FF0080
      80FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
      80FF000000FFFF00FF00FF00FF00000000FF00FFFFFF000000FF008080FF0080
      80FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0000
      00FFFF00FF00FF00FF00FF00FF00000000FF000000FF008080FF008080FF0080
      80FF008080FF008080FF008080FF008080FF008080FF008080FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FF000000FF000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0080
      80FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0080
      80FF008080FF008080FF008080FF008080FF008080FF008080FF008080FF0080
      80FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0000
      00FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00FF000000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0000
      00FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00FF000000
      00FF008080FF000000FFFF00FF00FF00FF00000000FF008080FF008080FF0000
      00FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00FF000000
      00FF008080FF000000FFFF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FFFF00FF00FF00
      FF00FF00FF00800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF008000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FFFF00
      FF00800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00800000FFFF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF8000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00800000FFFF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF8000
      00FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00800000FFFF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF8000
      00FF800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF008000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      00FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FF000000FFFF00FF00FF00FF00000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FF000000FFFF00FF00FF00FF00000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00800000FF800000FF800000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF000000FF0000
      00FFFF00FF00800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF800000FF800000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FFC0C0C0FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF800000FF800000FF000000FF000000FF000000FF000000FF0000
      00FFC0C0C0FF000000FFC0C0C0FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF800000FF800000FF000000FFFFFFFFFFFFFFFFFF000000FFC0C0
      C0FF000000FFC0C0C0FF000000FFC0C0C0FF000000FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF800000FF800000FF000000FFFFFFFFFF000000FFC0C0C0FF0000
      00FFFFFFFFFF000000FFC0C0C0FF000000FFC0C0C0FF000000FF000000FF0000
      00FFFF00FF00800000FF800000FF000000FFFFFFFFFF000000FF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFF000000FFC0C0C0FF000000FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF000000FF000000FFFFFF
      FFFF000000FF000000FF000000FF000000FF000000FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF000000FF000000FFFFFF
      FFFF000000FF000000FF000000FF000000FF000000FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00
      FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF000000FFFFFF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF000000FFFF0000
      FFFFFF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FFFF00FF000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00
      00FFFF0000FFFF0000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FF000000FF000000FF000000FF000000FF000000FF0000
      00FFFF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFC0C0
      C0FFFFFFFFFFFFFFFFFF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000FFFF0000FFFFFF00FF00FF00FF00FF00FF000000FFFFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFFFF00FF00FF0000FF0000FFFFFF00FF00FF00FF00FF00FF00FF00
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFFFF00FF00FF0000FF0000FFFFFF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFFFF00FF00FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFFFF0000FFFF0000FFFF00FF00FF00FF00FF0000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFFFF00
      FF00FF0000FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000FFFF0000FFFF0000
      FFFFFF0000FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      00FFFF0000FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FFFF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFF008000FF008000FF0080
      00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFF008000FF008000FF0080
      00FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFF008000FF008000FF0080
      00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFF0000FF0000FFFF0000FFFF0000
      FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00
      00FFFF0000FF000000FFFF00FF00000000FFFF0000FF0000FFFF0000FFFF0000
      FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFF0000FF000000FFFF00FF00000000FFFF0000FF0000FFFF0000FFFF0000
      FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00
      00FFFF0000FF000000FFFF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFFFF0000FFFF0000FFFF00
      00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFFFF0000FFFF0000FFFF00
      00FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFFFF0000FFFF0000FFFF00
      00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF000000FFFF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00800080FF000000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00800080FF808080FF800080FF000000FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00800080FF808080FF800080FF800080FF800080FF800080FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008000
      80FF808080FF800080FF800080FF800080FF800080FF800080FF800080FF8000
      80FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00800080FF8080
      80FF800080FF800080FF00FFFFFF00FFFFFF00FFFFFF008080FF800080FF8000
      80FF800080FF000000FFFF00FF00FF00FF00FF00FF00800080FF808080FF8000
      80FF800080FF008080FF008080FF800080FF00FFFFFF00FFFFFF800080FF8000
      80FF000000FF000000FFFF00FF00FF00FF00800080FF808080FF800080FF8000
      80FF800080FF800080FFC0C0C0FF00FFFFFF00FFFFFF800080FF800080FF0000
      00FF808080FF000000FFFF00FF00800080FF808080FF800080FF800080FF8000
      80FF800080FF00FFFFFF008080FF800080FF800080FF800080FF000000FF8080
      80FFC0C0C0FF000000FF000000FF800080FF000000FF000000FF800080FF8000
      80FF800080FF800080FF800080FF800080FF800080FF000000FF808080FFC0C0
      C0FFC0C0C0FF800080FF000000FF800080FF808080FF808080FF000000FF0000
      00FF800080FF800080FF800080FF800080FF000000FF808080FFC0C0C0FFC0C0
      C0FF800080FF000000FFFF00FF00800080FF808080FFFFFFFFFF808080FF8080
      80FF000000FF000000FF800080FF000000FF808080FFC0C0C0FFC0C0C0FF8000
      80FF000000FFFF00FF00FF00FF00000000FF808080FFC0C0C0FFFFFFFFFFFFFF
      FFFF808080FF808080FF000000FF808080FFC0C0C0FFC0C0C0FF800080FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF808080FFC0C0
      C0FFFFFFFFFFFFFFFFFF808080FFC0C0C0FFC0C0C0FF800080FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
      00FF808080FFC0C0C0FFFFFFFFFFC0C0C0FF800080FF000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000FF000000FF808080FF800080FF000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000FF000000FFFF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFF00FFFFFF
      FFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFFFFFF8080
      80FF808080FF808080FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFF00FFFFFF
      FFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFFFFFF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FFFFFFFFFF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFF00FFFFFF
      FFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFFFFFF8080
      80FF808080FF808080FF808080FF808080FF808080FF808080FFFFFFFFFF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FFFFFF00FFFFFF
      FFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFC0C0C0FF000000FFFF00FF00000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF000000FFFF00FF00FF00FF00000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF00FFFFFFFFFFFFFF00FF
      FFFF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FF00FFFFFFFFFFFFFF00FFFFFFFFFF
      FFFFFFFFFFFF00FFFFFF808080FF000000FF808080FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FF00FFFFFFFFFFFFFF00FFFFFFFFFF
      FFFFFFFFFFFF00FFFFFF000000FF008080FF000000FFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF00FFFFFFFFFFFFFF00FF
      FFFF00FFFFFFFFFFFFFF808080FF000000FF808080FF00FFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FF00FFFFFFFFFFFFFF00FFFFFFFFFF
      FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF00FFFFFFFFFFFFFF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00000000FFFFFFFFFF00FFFFFFFFFFFFFF0000
      00FF00FFFFFF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF00FFFFFFFFFFFFFF000000FF8080
      80FF000000FF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FFC0C0C0FF000000FF000000FFFFFFFFFF000000FF808080FF8080
      80FF808080FF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF000000FF000000FF000000FF808080FF808080FF000000FF8080
      80FF000000FF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF008080FF000000FF000000FF808080FF808080FF808080FF0000
      00FF808080FF000000FF008080FF008080FF000000FF000000FF000000FF0000
      00FF008080FF008080FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF008080FF008080FF008080FF008080FF008080FF0080
      80FF008080FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF008080FF000000FF000000FF000000FF000000FF0000
      00FF000000FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF008080FF000000FF000000FF000000FF000000FFC0C0
      C0FF000000FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF008080FF000000FF000000FF000000FF000000FFC0C0
      C0FF000000FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF000000FF000000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF800000FF800000FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFFC0C0C0FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFFFFFFFFFFC0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFFC0C0C0FF000000FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FFC0C0C0FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF000000FF000000FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF000000FF008080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FF000000FF008080FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFF000000FF008080FF008080FF000000FF000000FF000000FF0000
      00FF008080FF008080FF000000FF000000FFFFFFFFFFFFFFFFFFC0C0C0FFFFFF
      FFFFFFFFFFFF000000FF008080FF008080FF008080FF008080FF008080FF0080
      80FF008080FF008080FF000000FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FF000000FF008080FF000000FF000000FF000000FF000000FF0000
      00FF000000FF008080FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF008080FF000000FF000000FF000000FF000000FFC0C0
      C0FF000000FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF008080FF000000FF000000FF000000FF000000FFC0C0
      C0FF000000FF008080FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFF0000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000FFFFFF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
      00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF00808080FF000000FF000000FF000000FF8080
      80FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00
      00FFFF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00808080FF000000FF808080FFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000FFFF0000FFFF00
      00FFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000000FFFFFF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      00FFFF00FF00FF00FF00FF0000FF0000FFFFFF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      00FFFF00FF00FF00FF00FF0000FFFF00FF000000FFFFFF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00
      FF00FF00FF00FF0000FFFF00FF00FF00FF000000FFFFFF00FF00FF00FF00FF00
      FF00FF00FF00000000FFFF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF000000FFFFFF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF000000FFFFFF00FF00FF00FF000000FFFFFF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00
    }
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.HLR'
    Filter = 'All supported files (*.hlr, *.hgl, *.stx, *.txt)|*.hlr;*.hgl;*.stx; *.txt|UniHighlighter Rules (*.hlr)|*.hlr|UniHighlighter Old Format (*.hgl)|*.hgl|EditPlus syntax file (*.stx)|*.stx|UltraEdit syntax file (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    left = 16
    top = 160
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.HLR'
    Filter = 'UniHighlighter Rules (*.hlr)|*.hlr|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    left = 48
    top = 160
  end
  object popColorStd: TPopupMenu
    left = 48
    top = 96
  end
  object popColorAdv: TPopupMenu
    left = 80
    top = 96
  end
  object popColorSys: TPopupMenu
    left = 112
    top = 96
  end
  object listColors16: TImageList
    left = 48
    top = 128
  end
  object listColors40: TImageList
    left = 80
    top = 128
  end
  object listColorsSys: TImageList
    left = 112
    top = 128
  end
  object listRules: TImageList
    left = 16
    top = 128
  end
  object popSampleMemoMenu: TPopupMenu
    Images = listImages
    OnPopup = popSampleMemoMenuPopup
    left = 112
    top = 160
    object AddselectedtoKeywords1: TMenuItem
      Caption = 'Add to Keywords'
      OnClick = AddselectedtoKeywords1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Undo1: TMenuItem
      Caption = '&Undo'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF800000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF800000FF000000FF000000FF000000FF0000
        00FF800000FF000000FF000000FF000000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF800000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 7
      ShortCut = 16474
      OnClick = Undo1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cu&t'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF000000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF800000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF800000FF800000FF800000FF000000FF800000FF0000
        00FF000000FF800000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF8000
        00FF800000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF000000FF800000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 0
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = '&Copy'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FF000000FF000000FF000000FFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF000000FF0000
        00FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF000000FFFFFF
        FFFF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF000000FFFFFF
        FFFF000000FF000000FFFFFFFFFF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 1
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = '&Paste'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF800000FF800000FF800000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FF800000FF800000FF800000FFFFFFFFFF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFF800000FF8000
        00FF800000FFFFFFFFFF800000FF800000FF800000FF800000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFF800000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF800000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF800000FF800000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF808080FF008080FF800000FF800000FF800000FF8000
        00FF800000FF800000FF800000FF000000FF000000FF000000FF000000FF8080
        80FF008080FF808080FF008080FF808080FF008080FF808080FF008080FF8080
        80FF008080FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF808080FF808080FF000000FF000000FF000000FF000000FF8080
        80FF808080FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
        C0FF000000FF808080FF008080FF000000FF000000FF000000FF000000FF0080
        80FF808080FF008080FF000000FF00FFFFFF000000FF000000FF00FFFFFF0000
        00FF808080FF008080FF808080FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 2
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
        00FF000000FF000000FF000000FF000000FF000000FF000000FF
      }
      ImageIndex = 8
      OnClick = Delete1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select &All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
  end
  object OpenDialog2: TOpenDialog
    Filter = 'CLR files (*.clr)|*.clr|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    left = 144
    top = 160
  end
end

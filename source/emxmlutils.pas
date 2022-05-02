unit emXmlUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fasthtmlparser, laz2_DOM, ComCtrls;

type
  TXmlBeautifier = class
  private
    FIndentLevel: Integer;
    FList: TStrings;
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
    procedure FoundTextHandler(AText: String);
  protected
    function Indent(AText: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(AXmlText: String; AList: TStrings);
  end;

procedure BeautifyXml(AXmlText: String; AList: TStrings);
procedure XML2Tree(XMLDoc: TXMLDocument; ATreeView: TTreeView);


implementation

uses
  StrUtils;

{------------------------------------------------------------------------------}
{                             XML to tree                                      }
{------------------------------------------------------------------------------}

procedure XML2Tree(XMLDoc:TXMLDocument; ATreeView:TTreeView);

  // Local function that outputs all node attributes as a string
  function GetNodeAttributesAsString(pNode: TDOMNode):string;
  var
    i: integer;
  begin
    Result := '';
    if pNode.HasAttributes then
      for i := 0 to pNode.Attributes.Length -1 do
        with pNode.Attributes[i] do
          Result := Result + format(' %s="%s"', [NodeName, NodeValue]);

    // Remove leading and trailing spaces
    Result := Trim(Result);
  end;

  // Recursive function to process a node and all its child nodes
  procedure ParseXML(ANode:TDOMNode; ATreeNode: TTreeNode);
  begin
    // Exit procedure if no more nodes to process
    if ANode = nil then Exit;

    // Add node to TreeView
    ATreeNode := ATreeView.Items.AddChild(ATreeNode,
      Trim(ANode.NodeName + ' ' + GetNodeAttributesAsString(ANode) + ANode.NodeValue));

    // Process all child nodes
    ANode := ANode.FirstChild;
    while ANode <> Nil do
    begin
      ParseXML(ANode, ATreeNode);
      ANode := ANode.NextSibling;
    end;
  end;

begin
  ATreeView.Items.Clear;
  ParseXML(XMLDoc.DocumentElement,nil);
  ATreeView.FullExpand;
end;


{------------------------------------------------------------------------------}
{                            XML beautifier                                     }
{------------------------------------------------------------------------------}

procedure BeautifyXml(AXmlText: String; AList: TStrings);
begin
  with TXmlBeautifier.Create do
  try
    Execute(AXmlText, AList);
  finally
    Free;
  end;
end;


constructor TXmlBeautifier.Create;
begin
  FIndentLevel := -2;
end;

destructor TXmlBeautifier.Destroy;
begin
  inherited;
end;

procedure TXmlBeautifier.Execute(AXmlText: String; AList: TStrings);
var
  parser: THtmlParser;
begin
  FList := AList;
  FList.Clear;
  parser := THtmlParser.Create(AXmlText);
  try
    parser.OnFoundTag := @FoundTagHandler;
    parser.OnFoundText := @FoundTextHandler;
    parser.Exec;
  finally
    parser.Free;
  end;
end;

procedure TXmlBeautifier.FoundTagHandler(NoCaseTag, ActualTag: String);
begin
  // </something>
  if NoCaseTag[2] = '/' then begin
    FList.Add(Indent(ActualTag));
    dec(FIndentLevel);
  end else
  // <something ... />
  if NoCaseTag[Length(NoCaseTag)-1] = '/' then
    FList.Add(Indent(ActualTag))
  else
  // <something ...  >
  begin;
    inc(FIndentLevel);
    FList.Add(Indent(ActualTag));
  end;
end;

procedure TXmlBeautifier.FoundTextHandler(AText: String);
begin
  inc(FIndentLevel);
  FList.Add(Indent(AText));
  dec(FIndentLevel);
end;

function TXmlBeautifier.Indent(AText: String): String;
begin
  if FIndentLevel < 1 then
    Result := AText else
    Result := DupeString(' ', FIndentLevel*2) +  AText;
end;


end.


This file will contain a tutorial where in a explorative way working with XML files is explained.

hhzl

## Installation
````
Feature require: 'YAXO'. 
````

This YAXO package is included in the basic repository of Cuis Smalltalk. The code included is similar to the one in Squeak and Pharo Smalltalk as it goes back to the same original implementation.

## Writing a XML file
source: https://wiki.squeak.org/squeak/6338
adapted for Cuis, in particular the idiom
`stream := 'MyXml.xml' asFileEntry writeStream.`

````
doc := XMLDocument new.
stream := 'MyXml.xml' asFileEntry writeStream. "specify the stream"
writer := XMLWriter on: stream. "specify the writer"
writer initialize.
 
"Code below describes how you could write an xml element with a tag "
elmt := XMLElement named: 'user' attributes: Dictionary new.
 
"add the attribute tags and values to the "
childElmt := XMLElement named: 'name' attributes: Dictionary new.
childElmt addContent: (XMLStringNode string: ('rao')). "adding a tag called Name with the value 'rao'"
elmt addElement: childElmt.
 
doc addElement: elmt.
doc printXMLOn: writer.
stream close.
````

## Reading a XML file

### Reading 'MyXML.xml'
Reading back the file written above

````
XMLDOMParser  parseDocumentFrom: 'MyXml.xml' asFileEntry readStream
````

### Readig MusicXML hello world file
Reading a more complex example taken from the MusicXML website
https://www.w3.org/2021/06/musicxml40/tutorial/hello-world/

````
xmlString := 
'<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE score-partwise PUBLIC
    "-//Recordare//DTD MusicXML 4.0 Partwise//EN"
    "http://www.musicxml.org/dtds/partwise.dtd">
<score-partwise version="4.0">
  <part-list>
    <score-part id="P1">
      <part-name>Music</part-name>
    </score-part>
  </part-list>
  <part id="P1">
    <measure number="1">
      <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
        <clef>
          <sign>G</sign>
          <line>2</line>
        </clef>
      </attributes>
      <note>
        <pitch>
          <step>C</step>
          <octave>4</octave>
        </pitch>
        <duration>4</duration>
        <type>whole</type>
      </note>
    </measure>
  </part>
</score-partwise>'.

XMLDOMParser  parseDocumentFrom: xmlString readStream
````

### Reading a MusicXML file and extracting the notes
````
Feature require: 'YAXO'.
Feature require: 'Webclient'.

xmlString := (WebClient httpGet: 'https://raw.githubusercontent.com/DrCuis/Workbench/refs/heads/main/XML/FrereJacques.xml') content.

xmlDocument :=  XMLDOMParser  parseDocumentFrom: xmlString readStream.

noteElements := OrderedCollection new.
xmlDocument topElement tagsNamed: #note do: [:e | noteElements add: e].

"Show the notes"
noteElements explore.
````
Note: The Webclient package also requires the loading of the 'Sound' package.

Source: https://wiki.squeak.org/squeak/1559

### Equivalent of Pharo XMLDOMParser parseURL:

The equivalent of #parseURL: for the example given on the Pharo version of YAXO.
https://github.com/pharo-contributions/XML-XMLParser

```
(XMLDOMParser  parseDocumentFrom:  (WebClient httpGet: 'https://www.w3schools.com/xml/simple.xml') content readStream) explore
```
The porting effort of code using the Pharo version of YAXO is minimal.

## Exercise: explore DrGeo XML reading and writing code

### Steps
- Download current DrGeo
- Choose Desktop menu -> Tools -> SystemBrowser
- Search for class #XMLElement
- Have a look at the instance creation method of XMLElement (class side)
- Click on method named: aString attributes: attributeList
- Go for senders of it.

### The code
````
XMLNodeWithElements subclass: #XMLElement
	instanceVariableNames: 'name contents attributes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'YAXO'
````

You will find for example how an XMLElement is constructed to write out a rectangle as SVG

````
DrGSvgCanvas
rectangle: rect
	| rectNode |
	rectNode := XMLElement 
		named: #rect 
		attributes: {
			#x -> rect topLeft x asString . #y -> rect topLeft y asString .
			#width -> rect width asString . #height -> rect height asString } asDictionary.
	^ rectNode
````

And then this #rectangle: method is called by DrGSvgCanvas #fillRectangle:color:

````
fillRectangle: rect color: fillColor
	| rectNode |
	rectNode := self rectangle: rect.
	self styleOf: rectNode StrokeWidth: nil color: nil fillColor: fillColor.
	svgTree addElement: rectNode
````

More attributes are filled in for example, see

DrSvgCanvas

````
styleOf: element StrokeWidth: strokeWidth color: strokeColor strokeDashArray: sda strokeDashArrayOffset: sdao fillColor: fillColor
"
	Apply style to a given element (node) 
"
	strokeWidth ifNotNil: [element attributeAt: #'stroke-width' put: strokeWidth asString].
	strokeColor ifNotNil: [		element attributeAt: #stroke put: 
		(strokeColor isTransparent ifTrue: ['transparent'] ifFalse:		strokeColor hexHtml)].
	sda ifNotNil: [
		element 
			attributeAt: #'stroke-dasharray' 
			put: (String streamContents: [:s | sda do: [:e | s store: e] separatedBy: [ s space]])].
	sdao ifNotNil: [element attributeAt: #'stroke-dashoffset' put: sdao asString].
	fillColor 
		ifNotNil: [
			element attributeAt: #fill put: fillColor hexHtml.
			fillColor isOpaque ifFalse: [element attributeAt: #'fill-opacity' put: fillColor alpha asString] ]
		ifNil: [element attributeAt: #fill put: 'none']

````


pathNode := XMLElement named: #path.

Explore classes:

- DrGeoXML
- DrGSvgCanvas


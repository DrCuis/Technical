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

Reading back the file written above

````
XMLDOMParser  parseDocumentFrom: 'MyXml.xml' asFileEntry readStream
````

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

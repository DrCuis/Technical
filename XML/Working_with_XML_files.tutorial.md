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

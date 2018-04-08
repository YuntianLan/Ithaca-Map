import xml.etree.cElementTree as et
import json
import progressbar

data = {
    "nodes": [],
    "ways" : [],
}
"""
data -> {
    "nodes" : [{
        'lat' : <latitude>,
        'id'  : <id>,
        'lon' : <longitude>,
        'tags' : {
            k : v
        }
    }
    ],
    "ways" : [{
       'id' : <id>,
       'nodes' : {
            k : v
       }
       'tags' : {
            k : v
       }
    }]
}
"""
pbar = progressbar.ProgressBar()
t = et.ElementTree(file='map-full').getroot()
for i,child in pbar(list(enumerate(t))):
    if child.tag == 'node':
        node_data = {k:v for k,v in child.attrib.iteritems() \
                if k in ['lat', 'id', 'lon']}
        node_data['tags'] = {}
        for grandchild in child:
            if 'tags' not in node_data:
                node_data['tags'] = {}
            node_data['tags'][grandchild.attrib['k']] = grandchild.attrib['v']
        data['nodes'].append(node_data)

    elif child.tag == 'way':
        way_data = {k:v for k,v in child.attrib.iteritems() \
                if k in ['id']}
        way_data['nodes'] = []
        way_data['tags'] = {}
        for grandchild in child:
            if grandchild.tag == 'nd':
                way_data['nodes'].append(grandchild.attrib['ref'])
            elif grandchild.tag == 'tag':
                way_data['tags'][grandchild.attrib['k']] = grandchild.attrib['v']

        data['ways'].append(way_data)



with open('full.json', 'w') as f:
	json.dump(data, f)

## parse_europarl_amend.py
## Code to parse European parliament amendments
## as represented in plaintext
## Takes as input plain text files, returns a list of amendments in plaintext

import csv
import os
import re
import time
from xml.etree import ElementTree as ET
from xml.parsers.expat import ExpatError 

os.chdir('/Users/markhuberty/Documents/Research/Dissertation/master/notes/leg_hist_sources/intl_mkt/2007')

# conn = open('/Users/markhuberty/Downloads/test.htm')#ep_draft_legislation_first_reading.txt', 'rb')

conn = open('../2003/ep_first_reading_report.htm')

input_string = ''
for row in conn:
    input_string += (row + ' ')

conn.close()

##  Try to parse the tables out
## First dump everything before the first amendment
re_amend_split = re.compile('LEGISLATIVE PROPOSAL')
re_table = re.compile('<table class=MsoNormalTable.*?>(.*?)</table>',
                      re.DOTALL)

input_string = re_amend_split.split(input_string,  maxsplit=2)
input_string = input_string.pop(2)
test_match = re_table.findall(input_string)

re_amend_table = re.compile('(<p class=MsoNormal.*?>)|(<p class=Normal.*?>)', re.DOTALL)
re_html = re.compile('<.*?>', re.DOTALL)
re_nbsp = re.compile('&nbsp;')
re_emdash = re.compile('&#8209;')

amendments = []
for group in test_match:
    amend_temp = re_amend_table.split(group, maxsplit=2)
    amend_temp = amend_temp.pop()
    amend_temp = re_multispace.sub(' ', amend_temp)
    amend_temp = re_nbsp.sub(' ', amend_temp)
    amend_temp = re_html.sub(' ', amend_temp)
    amend_temp = re_multispace.sub(' ', amend_temp)
    amendments.append(amend_temp)

## This works for word docs converted to htm
def eu_report_to_amend(input_string,
                       preamble_regex,
                       table_regex,
                       amend_regex,
                       clean_regex,
                       replace_regex,
                       replace_strings,
                       preamble_maxsplit=1,
                       amend_maxsplit=1):
    

    amend_raw = preamble_regex.split(input_string,
                                     maxsplit=preamble_maxsplit
                                     )
    amend_raw = amend_raw.pop()

    amend_match = table_regex.findall(amend_raw)

    amendments = []
    for a in amend_match:
        a = amend_regex.split(a, maxsplit=amend_maxsplit)
        a = a.pop()
        if clean_regex is not None:
            for r in clean_regex:
                a = r.sub(' ', a)
        if replace_regex is not None:
            for i, r in enumerate(replace_regex):
                a = r.sub(replace_strings[i], a)
        amendments.append(a)

    return(amendments)

## Works below
## Assumes: html translated from ms word document
## Cleans out the cruft, tabulates, and extracts amendments
conn = open('../2003/ep_first_reading_report.htm')
#conn = open('../1996/ep_second_reading_report.html.htm')

input_string = ''
for row in conn:
    input_string += (row + ' ')

conn.close()

re_tables = re.compile('<table.*?>.*?</table>', re.DOTALL)
re_clean_tables = re.compile('</{0,1}p.*?>', re.DOTALL)
re_clean_table_tags = re.compile('\s[a-z]*?=.*?(?=>)', re.DOTALL)
re_clean_td_tags = re.compile('\swidth=.*?(?=>)', re.DOTALL)
re_clean_style = re.compile('\sstyle=.*?(?=>)', re.DOTALL)
re_span = re.compile('</{0,1}span.*?>', re.DOTALL)
re_newline = re.compile(r'\n')
re_fn = re.compile('<a\s{0,}><!.*?>.*?<!.*?></a>', re.DOTALL)
re_markup = re.compile('</{0,1}[rbiusp:o]*?\s*?>')
re_markup_links = re.compile('</{0,1}a.*?>')
#re_unicode_quote = re.compile('\\xd5|\\xd4')
re_unicode_quote = re.compile('\\xd5|\\xd4|\\xd3|\\xd2')
#re_unicode = re.compile(ur'\xd[0-9]')
re_amp_quote = re.compile('&[a-z0-9A-Z]*?;')

## This is necessary to clean out all the bad cruft
## from the word html
test = re_tables.findall(input_string)
test = [re_clean_table_tags.sub('', element) for element in test]
test = [re_clean_tables.sub('', element) for element in test]
test = [re_clean_td_tags.sub('', element) for element in test]
test = [re_clean_style.sub('', element) for element in test]
test = [re_span.sub('', element) for element in test]
test = [re_newline.sub('', element) for element in test]
test = [re_fn.sub('', element) for element in test]
test = [re_markup.sub('', element) for element in test]
test = [re_nbsp.sub('', element) for element in test]
test = [re_unicode_quote.sub('', element) for element in test]
test = [re_markup_links.sub('', element) for element in test]
test = [re_amp_quote.sub('', element) for element in test]

def parse_amend_html(string,
                     re_tables,
                     re_clean
                     ):
    ## Find the tables and extract
    tables = re_tables.findall(input_string)

    ## Clean the junk in the tables
    for fun in re_clean:
        tables = [fun.sub('', t) for t in tables]

    ## Parse the clean html 
    tab_html = []
    for element in tables:
        try:
            tab_html.append(ET.XML(element))
        except ExpatError:
            continue

    ## Extract the full table
    tab_list = []
    for element in tab_html:
        element = iter(element)
        output = []
        for row in element:
            this_row = [col.text for col in row]
            output.append(this_row)
        tab_list.append(output)

    ## Extract the parliamentary stuff
    ## Assumes the parl amends are always column 2
    parl_amend = []
    for tab in tab_list:
        this_amend = []
        for row in tab:
            try:
                this_amend.append(row[1])
            except IndexError:
                continue
        if this_amend:
            parl_amend.append(this_amend)

    return parl_amend

test_fun_out = parse_amend_html(input_string,
                                re_tables,
                                [re_clean_tables, re_clean_table_tags, re_span, re_newline, re_fn, re_markup, re_markup_links, re_unicode_quote, re_amp_quote]
                                )
    

test_html = []
for idx, element in enumerate(test):
    print idx
    try:
        test_html.append(ET.XML(element))
    except ExpatError:
        print 'failed'
        continue

test_html_to_tab = []
for idx, element in enumerate(test_html):
    print idx
    entry = iter(element)
    output = []
    for row in entry:
        this_row = [col.text for col in row]
        output.append(this_row)
    test_html_to_tab.append(output)

parl_proposals = []
for idx, tab in enumerate(test_html_to_tab):
    print idx
    this_proposal = []
    for row in tab:
        try:
            this_proposal.append(row[1])
        except IndexError:
            continue
    parl_proposals.append(this_proposal)
## Works above

    
test_html = [ET.XML(element) for element in test]
test_tab = [iter(element) for element in test]

for row in rows:
    values = [col.text for col in row]
    print dict(zip(headers, values))

re_find_amend = re.compile('Amendment.\s[<.*?>]*?[0-9]{1,}',
                           re.S | re.M)

re_amend_split = re.compile('LEGISLATIVE PROPOSAL')
re_numbering = re.compile('^\s*\({0,1}[a-z0-9\s\-]*\){0,1}\.{0,1}')
eu_amend = eu_report_to_amend(input_string,
                              re_amend_split,
                              re_table,
                              re_amend_table,
                              [re_multispace,
                               re_nbsp, re_html,
                               re_numbering,
                               re_multispace],
                              [re_emdash],
                              ['-'],
                              preamble_maxsplit=2,
                              amend_maxsplit=2
                              )
## NOTE - this doesn't fully work for entites with >1 col
## and entries in both cols. Would be better to try and
## parse the entire list into a set of tables and then
## load the tables with something like the xml lib.
 
conn = open('../2003/ep_first_report_amendments.txt', 'wb')
for iter in eu_amend:
    conn.write(iter + '\n')
conn.close()

input_string = ''

conn = open('../2003/ep_second_reading_report.htm')

for row in conn:
    input_string += (row + ' ')

conn.close()


eu_amend_2 = eu_report_to_amend(input_string,
                                re_amend_split,
                                re_table,
                                re_amend_table,
                                [re_multispace,
                                 re_nbsp, re_html,
                                 re_multispace],
                                [re_emdash],
                                ['-'],
                                preamble_maxsplit=2,
                                amend_maxsplit=2
                                )
## Works to here...

                      
input_string = ''

conn = open('../1996/ep_second_reading_report.html.htm')

for row in conn:
    input_string += (row + ' ')

conn.close()

re_amend_split = re.compile('Amendments by Parliament')
eu_amend_3 = eu_report_to_amend(input_string,
                                re_amend_split,
                                re_table,
                                re_amend_table,
                                [re_multispace,
                                 re_nbsp, re_html,
                                 re_multispace],
                                [re_emdash],
                                ['-'],
                                preamble_maxsplit=2,
                                amend_maxsplit=2
                                )

def amend_to_paragraphs(amend_list):
    

## Rough process:
## drop preamble
## Split at committee header
## Split at amendment+number
## split at amendment and take index 2
## split at () and take index 2
## r


def iterative_split(string, split_strings, keep_element):
    for idx, split in enumerate(split_strings):
        new_string = []

        if idx == 0:
            temp_string = re.split(split, string) ## assume you start w/ single str
            if keep_element[idx]:
                temp_string = temp_string[keep_element[idx]]
            print len(temp_string)
            new_string = temp_string
            
        else:
            for j in range(len(string)):
                print j
                temp_string = re.split(split, string[j])
                #print temp_string
                print len(temp_string)

                if type(temp_string) == list:
                    if len(temp_string) > 1:
                        if keep_element[idx] is not None:
                            out_string = temp_string[keep_element[idx]]
                            new_string.append(out_string)
                        else:
                            new_string.extend(temp_string)
                    else:
                        new_string.extend(temp_string)
                
        string = new_string
        print(len(string))
            
    return(string)

def read_amendment_file(conn,
                        re_preamble,
                        clean_regexp,
                        postsplit_regexp,
                        amend_splits,
                        amend_split_drops,
                        amend_sep_regexp,
                        preamble_header):
    dict_out = {}
    input_doc = ''
    for row in conn:
        ## Append the string
        input_doc += (row + ' ')
    conn.close()
    print 'File loaded'

    ## Split the doc at the preamble
    input_doc_split = re_preamble.split(input_doc, maxsplit=1)[1]
    preamble = re_preamble.match(input_doc)
    preamble = preamble.group(0)
    
    print 'Preamble chopped'
    print type(input_doc_split)
    
    ## Clean out if regexp provided
    if clean_regexp is not None:
        for fun in clean_regexp:
            input_doc_split = fun.sub(' ', input_doc_split)

    print 'Splitting'
    ## Then iteratively split
    iter_split = iterative_split(input_doc_split,
                                 amend_splits,
                                 amend_split_drops
                                 )

    ## Then clean for the actual amendment

    if amend_sep_regexp is not None:
        for idx in range(len(iter_split)):
            amend_split = amend_sep_regexp.split(iter_split[idx])
            if len(amend_split) > 1:
                iter_split[idx] = amend_split.pop()
            else:
                iter_split[idx] = amend_split[0]
            
    ## Do post-cleanup
    if postsplit_regexp is not None:
        for fun in postsplit_regexp:
            for idx in range(len(iter_split)):
                iter_split[idx] = fun.sub(' ', iter_split[idx])
                         
    return(iter_split)
    


amend_splits = ['AMENDMENTS',
                'Amendment\s*[0-9]{1,}\n',
                'Amendment\n',
                'Justification'# ,
                # '\.\n \n'# ,
                # '(\([0-9]{1,}a-z]{0,}\)) |([0-9a-z]+?\.)' 
                ]

## This last doesn't parse correctly
## Can't use 4a. or (4), can also get 4 a.
## Can't break by paragraphs
## Would be much better to figure out how to
## put the "Amendment" tag in the right place to begin with.

re_preamble = re.compile('^.*?(?=Amendment)', re.DOTALL)

# input_string_test = re_preamble.split(input_string, maxsplit=1)

re_procedure = re.compile('PROCEDURE.*(?=AMENDMENTS)', re.DOTALL)
re_newline = re.compile('\n')
re_multispace = re.compile('\s{1,}')
re_end = re.compile('PROCEDURE.*$', re.DOTALL)
re_quote = re.compile('\"')

re_amend = re.compile('(\.|;)\n\s\n\s*?(?=\({0,1}[0-9a-z])')
# input_string_test = re_procedure.sub('',
#                                      input_string_test[1]
#                                      )

# test_split = iterative_split(input_string_test[0:2000],
#                              amend_splits,
#                              [None, None, 1, 0]
#                              )

conn = open('ep_draft_legislation_first_reading.txt', 'rb')
test = read_amendment_file(conn,
                           re_preamble=re_preamble,
                           clean_regexp=[re_procedure, re_end],
                           postsplit_regexp=[re_multispace],
                           amend_splits=amend_splits,
                           amend_split_drops=[None, None, 1, 0],
                           amend_sep_regexp=re_amend,
                           preamble_header='Amendment'
                           )

conn2 = open('../2003/ep_first_reading_report_PE309.068', 'rb')
test2 = read_amendment_file(conn2,
                            re_preamble=re_preamble,
                            clean_regexp=[re_procedure, re_end, re_quote],# re_multispace],
                            postsplit_regexp=[re_multispace],
                            amend_splits=amend_splits,
                            amend_split_drops=[None, None, 1, 0],
                            amend_sep_regexp=re_amend,
                            preamble_header='Amendment'
                            )


## do some checking
for t in test:
    print t
    time.sleep(1)
    

## This doesn't work b/c it grabs both the commission paragraph
## and amendment. Ordering is "Text proposed by the Commission", "Amendment",
## "commission text", "amendment text")

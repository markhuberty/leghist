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
import copy

os.chdir('/Users/markhuberty/Documents/Research/Dissertation/master/notes/leg_hist_sources/')

## Function definition
## NOTE: the functions defined below work for the following cases:
## EU Parliamentary amendments converted from MS Word format to html
## EU legislation dumped from the website as plaintext

## partition
## input: a list and indices for where to partition the list
## output: a list of lists, of length len(indices)+1


def partition(alist, indices):
    return [alist[i:j] for i, j in zip([0]+indices, indices+[None])]

## parse_amend_html
## Input: a string, read in from the html document of the
##   parliamentary amendments, which itself was constructed
##   from the EU parliament MS Word document via the 'save as htm' feature.
##        a regexp that will identify and return the tables in the string
##        a list of regexp to clean up the html inside the tables
## Output: a list of parliamentary amendments, represented as a
##   list of lists, each element of each list is a paragraph of the
##   amendment as it appeared in the table cells
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
    ## Assumes the parl amends are always the last column
    parl_amend = []
    for tab in tab_list:
        this_amend = []
        for row in tab:
            try: ## Drop blanks if they aren't trapped before
                if(len(row) > 1 and len(re.sub(' ', '', row[len(row) -1 ])) > 0):
                    this_amend.append(row[len(row) -1 ])
            except IndexError:
                continue
        if this_amend:
            d_key = ['Paragraph ' + str(idx) for idx, p in enumerate(this_amend)]
            dict_amend = dict(zip(d_key, this_amend))
            
            parl_amend.append(dict_amend)

    return parl_amend


## parse_legislation
## Input: string: a string of legislation taken from plaintext
##        assumes that the
##        preamble_string: a string demarcating the end of the leg. preamble (if any)
##        article_string: a string denoting the breaks between leg. articles
##        section_string: a string denoting the breaks between sections in articles
##        cleanup_strings: a list of strings to be cleaned out of the text
##        NOTE: all strings can be regexp
## Output: a 2-element list with the preamble and list of articles split by section
def parse_legislation(string,
                      preamble_string,
                      article_string,
                      section_string,
                      cleanup_strings
                      ):

    ## Split off the preamble
    preamble_split = re.split(preamble_string, string, maxsplit=1)

    ## Split out the articles
    article_split = re.split(article_string, preamble_split[1])

    articles = []
    for a in article_split:
        ## For each article, split by section
        split_articles = re.split(section_string, a)

        ## If it's not empty, clean it up
        if split_articles:
            if cleanup_strings:
                for idx, s in enumerate(split_articles):
                    for c in cleanup_strings:
                        split_articles[idx] = re.sub(c, '', split_articles[idx])
                        
            articles.append(split_articles)
        else:
            ## split didn't matter (only one section)
            ## so clean up anyway
            if cleanup_strings:
                for c in cleanup_strings:
                   a  = re.sub(c, '', a)

            ## Append either the single article or the list of article sections
            articles.append(a)

    out = {'preamble': preamble_split[0],
           'articles': articles
           }
    return(out)



## extract_amend_numbers
## Input: string: an html record of the amendment
##        re_amend: the regexp for identifying the amendment in the string
##        cleanup_regexp: a list of regular expressions to clean up the
##          output of re_amend
##        committee_names: a list of committee names that provided the amendments.
##          Should be passed in the order the amendments appear in the document
##        NOTE: assigning committee names assumes that each committee submits amendments
##              titled Amendment 1:N.
## Output: a dict that maps committee_name to the amendments Amendment 1:N that it submitted,
##         in the order as they appeared in the input_string
def extract_amend_numbers(string, re_amend, clean_regexp, committee_names):
    amend_labels = re_amend.findall(string)

    if clean_regexp:
        for r in clean_regexp:
            amend_labels = [r.sub('', a) for a in amend_labels]

    ## Get the amend1 locations:
    dup_index = []
    for i, a in enumerate(amend_labels):
        if re.match('Amendment\s{1,}1{1}$', a):
            dup_index.append(i)
    dup_index.pop(0)

    amend_labels = partition(amend_labels, dup_index)

    amend_labels = dict(zip(committee_names, amend_labels))
    
    return(amend_labels)


## paired_amend_fun
## Input: input_string: a string containing the full amendment text as html
##        re_tables: a regexp indicating how to extract tables from the HTML
##        re_amend_cleanup: a list of regexp to clean up amendment tables
##        re_amend_header: a regexp to extract the amendment numbers
##        re_amend_header_cleanup: a list of regexp to clean up the amendment numbers
##        committee_names: a list of committees that submitted amendments in input_string,
##                         in the order those amendments were submitted
## Output: a list of lists. Each element is formatted [committee, amendment, paragraph, text]

def paired_amend_fun(input_string,
                     re_tables,
                     re_amend_cleanup,
                     re_amend_header,
                     re_amend_header_cleanup,
                     committee_names
                     ):

    amendments = parse_amend_html(input_string,
                                  re_tables,
                                  re_amend_cleanup
                                  )

    amendment_headers = extract_amend_numbers(input_string,
                                              re_amend_header,
                                              re_amend_header_cleanup,
                                              committee_names
                                              )

    ## Then write into a list of form:
    ## committee, amendment, paragraph, text
    out_labels = []
    for key in sorted(amendment_headers.keys()):
        for a in amendment_headers[key]:
            out_labels.append([key, a]) ## fails here b/c index is wrong


    out_paras = []
    for element in amendments:
        this_para = [[k, v] for k, v in iter(sorted(element.iteritems()))]
        out_paras.append(this_para)


    out_list = []
    for l, m in zip(out_labels, out_paras):
        for p in m:
            lab = list(l)
            lab.extend(p)
            out_list.append(lab)
    
    return(out_list)




## End function definition

## Regexp for use in identifying and cleaning the input strings
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
re_semicolon = re.compile(';')
re_endash = re.compile('&#8211|&#8209')

re_amend =  re.compile('Amendment\s*?</{0,1}span.*?[0-9]{1,}(?=<)', re.DOTALL)
re_amend_clean = re.compile('</{0,1}span.*>', re.DOTALL)
re_amp = re.compile('&.*?(?=[0-9])')
## End regexp definition
 

## START TESTS

## Parse first reading report from 2003 
conn = open('./intl_mkt/2003/ep_first_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()


ep_2003_first_reading = parse_amend_html(input_string,
                                         re_tables,
                                         [re_clean_tables, re_clean_table_tags, re_span, re_newline, re_fn, re_markup, re_markup_links, re_unicode_quote, re_amp_quote, re_semicolon, re_endash]
                                         )

## Parse second reading report from 2003 
conn = open('./intl_mkt/2003/ep_second_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()


ep_2003_second_reading = parse_amend_html(input_string,
                                          re_tables,
                                          [re_clean_tables, re_clean_table_tags, re_span, re_newline, re_fn, re_markup, re_markup_links, re_unicode_quote, re_amp_quote, re_amp, re_semicolon]
                                          )

## Parse second reading report from 1996  
conn = open('./intl_mkt/1996/ep_second_reading_report.html.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

ep_1996_second_reading = parse_amend_html(input_string,
                                          re_tables,
                                          [re_clean_tables, re_clean_table_tags, re_span, re_newline, re_fn, re_markup, re_markup_links, re_unicode_quote, re_amp_quote, re_amp, re_semicolon]
                                          )



## TEST LEGISLATION
preamble_string = 'HAVE ADOPTED THIS DIRECTIVE'
article_string = 'Article\s[0-9]{1,}[ ]*\n'
section_string = '\s*?[0-9]{1,}\.'
cleanup_strings = ['\n']

conn = open('./intl_mkt/2003/directive_2003_54_ec')
input_string = ''
for row in conn:
    input_string += (row + ' ')

conn.close()


directive_2003_54_ec = parse_legislation(input_string,
                                         preamble_string,
                                         article_string,
                                         section_string,
                                         cleanup_strings
                                         )


conn = open('./intl_mkt/2007/directive_2009_72_ec')
input_string = ''
for row in conn:
    input_string += (row + ' ')

conn.close()

directive_2009_72_ec  = parse_legislation(input_string,
                                          preamble_string,
                                          article_string,
                                          section_string,
                                          cleanup_strings
                                          )

## This is a council proposal for consolidated leg
conn = open('./rese/2001/com_2000_279')

input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

com_2000_279  = parse_legislation(input_string,
                                  preamble_string,
                                  article_string,
                                  section_string,
                                  cleanup_strings
                                  )

## This is a council proposal for consolidated leg
## Parsed from the OCR'd council PDF via datasciencetoolkit
conn = open('/Users/markhuberty/Downloads/council_propsal.pdf.txt')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

## Had to cleanout some initial cruft
input_string = re.sub('\\xc2|\\xa0|\\xe2|\\x80|\\xa6', ' ', input_string)
input_string = re.sub('  ', ' ', input_string)

council_proposal_intl_mkt_2007 = parse_legislation(input_string,
                                                   preamble_string,
                                                   article_string,
                                                   section_string,
                                                   cleanup_strings
                                                   )


## Test the amendment header parser
conn = open('./intl_mkt/2003/ep_first_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

ep_amendment_headers_first_reading_2003 = extract_amend_numbers(input_string,
                                                                re_amend,
                                                                [re_amend_clean, re_newline],
                                                                ['A', 'B', 'C', 'D']
                                                                )

conn = open('./intl_mkt/2003/ep_second_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

ep_amendment_headers_second_reading_2003 = extract_amend_numbers(input_string,
                                                                 re_amend,
                                                                 [re_amend_clean, re_newline],
                                                                 ['A']
                                                                 )


## Finally, test the combined function and write out a csv record

conn = open('./intl_mkt/2003/ep_first_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

amend_list = paired_amend_fun(input_string,
                              re_tables,
                              [re_clean_tables, re_clean_table_tags, re_span, re_newline, re_fn, re_markup, re_markup_links, re_unicode_quote, re_amp_quote, re_semicolon, re_endash],
                              re_amend,
                              [re_amend_clean, re_newline],
                              ['A', 'B', 'C', 'D']
                              )

conn = open('./intl_mkt/2003/ep_first_reading_parsed.txt', 'wb')
writer = csv.writer(conn)
writer.writerow(['committee', 'amendment', 'paragraph', 'text'])
for item in amend_list:
    writer.writerow(item)
conn.close()

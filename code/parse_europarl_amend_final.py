## parse_europarl_amend.py
## Code to parse European parliament amendments
## as represented in plaintext
## Takes as input plain text files, returns a list of amendments in plaintext

import csv
import re
import time
from xml.etree import ElementTree as ET
from xml.parsers.expat import ExpatError
import copy
import urllib
import BeautifulSoup as BeautifulSoup

## Function definition
## NOTE: the functions defined below work for the following cases:
## EU Parliamentary amendments converted from MS Word format to html
## EU legislation dumped from the website as plaintext

## This works really well for amendments parsed from their word documents
## amend_html should be a html string of the entire document.
def soup_amend_parser(amend_html, split_string='Amendment',
                      clean_string='&nbsp;|&quot;|\n'):
    a_split = re.split(split_string, amend_html)
    amendments = []
    for a in a_split:
        temp = BeautifulSoup.BeautifulSoup(a)
        temp_rows = temp.findAll('td')
        temp_left_col = [t for t in temp_rows if re.search('border-left', str(t))]

        if len(temp_left_col) > 0:
            amend = [t.findAll(text=True) for t in temp_left_col]
            amendments.append(amend)
    print 'outputing labeled amendments'
    labeled_amendments = []
    for idx, a in enumerate(amendments):
        for jdx, s in enumerate(a):
            s_out = ''.join(s).encode('utf-8')
            s_out = re.sub(clean_string, '', s_out)

            if len(s_out) > 0:
                out = {'amendment': idx,
                       'paragraph': jdx,
                       'text': s_out
                       }
                labeled_amendments.append(out)
    return(labeled_amendments)

## Basic idea:
## 1. Extract all the rows
## 2. For each row, get the cols
## 3. Get the second col out of each of them
## 4. Return the text
def soup_amend_parser_2(amend_html, split_string='Amendment',
                        clean_string='&nbsp;|&quot;|\n'):
    soup = BeautifulSoup.BeautifulSoup(amend_html)

    rows = soup.findAll('tr')
    cols = []
    amend_nums = []
    section_nums = []
    for idx, r in enumerate(rows):
        temp = r.findAll('td')
        if len(temp) > 1:
            r_txt = temp[-1].findAll(text=True)
            #r_txt = re.sub(clean_string, '', r_txt)
            cols.append(''.join(r_txt))
            amend_nums.append(idx)
            section_nums.append(range(len(r_txt)))

    ## Get the amendment locations
    
    return([cols, amend_nums, section_nums])

def soup_amend_parser_3(amend_html, clean_string='&nbsp;|&quot;|\n'):
    soup = BeautifulSoup.BeautifulSoup(amend_html)

    tab = soup.findAll('table')

    out = []
    amend_idx = 1
    for idx, t in enumerate(tab):
        rows = t.findAll('tr')
        has_amend = False
        section_idx = 1
        for jdx, r in enumerate(rows):
            temp = r.findAll('td')
            if len(temp) > 1:
                has_amend=True
                r_txt = temp[-1].findAll(text=True)
                col = re.sub(clean_string, '',''.join(r_txt).encode('utf-8'))
                if len(col) > 0:
                    out.append({'amendment': amend_idx,
                                'paragraph': section_idx,
                                'text': col})
                    section_idx += 1
        if has_amend:
            amend_idx += 1
    return(out)

                
    cols = []
    amend_nums = []
    section_nums = []
    for idx, r in enumerate(rows):
        temp = r.findAll('td')
        if len(temp) > 1:
            r_txt = temp[-1].findAll(text=True)
            r_txt = re.sub(clean_string, '', ''.join(r_txt))
            cols.append(r_txt)
            amend_nums.append(idx)
            section_nums.append(range(len(r_txt)))

    ## Get the amendment locations
    
    return([cols, amend_nums, section_nums])

        
    a_split = re.split(split_string, amend_html)
    amendments = []
    for a in a_split:
        temp = BeautifulSoup.BeautifulSoup(a)
        temp_rows = temp.findAll('tr')
        
        temp_cols = []
        for r in temp_rows:
            tc = temp_rows.findAll('td')
            
        temp_cols = temp_rows.findAll('td')
        
        if len(temp_cols) > 0:
            amend = [t.findAll(text=True) for t in temp_cols if len(t) > 0]
            amendments.append(amend)
    return(amendments)

    print 'outputing labeled amendments'
    labeled_amendments = []
    for idx, a in enumerate(amendments):
        for jdx, s in enumerate(a):
            s_out = ''.join(s).encode('utf-8')
            s_out = re.sub(clean_string, '', s_out)

            if len(s_out) > 0:
                out = {'amendment': idx,
                       'paragraph': jdx,
                       'text': s_out
                       }
                labeled_amendments.append(out)
    return(labeled_amendments)


## partition
## input: a list and indices for where to partition the list
## output: a list of lists, of length len(indices)+1


def partition(alist, indices):
    return [alist[i:j] for i, j in zip([0]+indices, indices+[None])]

    

## parse_amend_html_doc
## Input: a string, read in from the html document of the
##   parliamentary amendments, which itself was constructed
##   from the EU parliament MS Word document via the 'save as htm' feature.
##        a regexp that will identify and return the tables in the string
##        a list of regexp to clean up the html inside the tables
## Output: a list of parliamentary amendments, represented as a
##   list of lists, each element of each list is a paragraph of the
##   amendment as it appeared in the table cells
def parse_amend_html_word(input_string,
                          re_tables,
                          re_clean
                          ):
    ## Find the tables and extract
    tables = re_tables.findall(input_string)

    ## Clean the junk in the tables
    for fun in re_clean:
        tables = [fun.sub('', t) for t in tables]

    tables = [unicode(t, errors='ignore') for t in tables]
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
        for idx, row in enumerate(tab):
            try: ## Drop blanks if they aren't trapped before
                if(len(row) > 1 and len(re.sub(' ', '', row[len(row) -1 ])) > 0
                   and re.sub(" ", "", row[-1]) != 'Amendment'):
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
                   a  = re.sub(c, ' ', a)

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
            amend_labels = [r.sub(' ', a) for a in amend_labels]

    ## Get the amend1 locations:
    dup_index = []
    for i, a in enumerate(amend_labels):
        if re.match('Amendment\s{1,}1{1}$', a):
            dup_index.append(i)

    print dup_index
    if len(dup_index) > 1:
        dup_index.pop(0)
    else:
        dup_index = None

    if dup_index:
        amend_labels = partition(amend_labels, dup_index)

    ## Breaks here, b/c the dict sorts by committee_name and
    ## thus spits out in the wrong order
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

    amendments = parse_amend_html_word(input_string,
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
    for key in committee_names:
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
re_unicode_quote = re.compile('\\xd5|\\xd4|\\xd3|\\xd2|\x92|\xb4')
#re_unicode = re.compile(ur'\xd[0-9]')
re_amp_quote = re.compile('&[a-z0-9A-Z]*?;')
re_semicolon = re.compile(';')
re_endash = re.compile('&#8211|&#8209')

re_amend =  re.compile('Amendment\s*?</{0,1}span.*?[0-9]+?(?=<)', re.DOTALL)
re_amend_clean = re.compile('</{0,1}span.*>', re.DOTALL)
re_amp = re.compile('&.*?(?=[0-9])')
## End regexp definition

 
## parse_w3m_output
## Takes as input the text dump of a 2-col file by the w3m browser
## Returns a list of amendments with amendment numbers
## Most suitable for parsing the parliamentary first reading reports
## with amendments from the EU Parliament, which come in 2-col format
## Inputs:
## doc: the raw text dump
## amend_idx: the column index where the amendment records start. If doc_type is 'clean_html' this is not required; the code will try to parse it directly
## doc_type: either 'clean_html' for html dumped directly from
##           the Parliament; or 'word' if the html was produced by
##           saving the equivalent MS WORD file as .html
## amend_string: a regex string indicating where Amendment + number
## is to be found
## end_string: a regex string indicating where non-Amendment data
## starts in the text (i.e., if Amendment + text is followed by Justification + text, then 'Justification' would be used.
def parse_w3m_output(doc,
                     amend_idx=None,
                     clean_html=True,
                     amend_string = 'Amendment\s*?[0-9]+?(?=\n)',
                     end_string = 'Justification|PROCEDURE',
                     committee_names=None,
                     len_threshold=0
                     ):

    print 'Getting amendment indices'
    if clean_html:
        for row in doc:
            com_test = re.search('Text proposed by the Commission', row)
            amend_test = re.search('Amendment', row)
            
            if com_test and amend_test:
                amend_idx = amend_test.span()
                amend_idx = amend_idx[0]
                break
    elif not clean_html and amend_idx is None:
        return 'If not clean_html, then must supply the column index for amendment paragraph justification'

    print 'Getting amendment interval indices'
    idx_breaks = []
    for idx, row in enumerate(doc):
        has_amend = re.search(amend_string, row)
        has_just = re.search(end_string, row)

        if has_amend:
            idx_breaks.append(['A', idx])
        if has_just:
            idx_breaks.append(['J', idx])

    print 'Getting amendments'
    amendments = []
    for i, breaks in enumerate(idx_breaks):
        if breaks[0] == 'A':
            amend = []
            for j in range(breaks[1], idx_breaks[i+1][1]):
                amend.append(doc[j])
            amendments.append(amend)
            
        elif breaks[0] == 'J':
            continue

    print 'Concatenating amendments'
    amendments_final = []
    for a in amendments:      
        this_amend = {}
        para_counter = 0
        amend_text = ''
        for i, r in enumerate(a):
            if i > 0:
                prior_not_empty = len(re.sub("[\s0-9]", "", a[i-1])) > 0
            else:
                prior_not_empty = True
                
            current_not_empty = len(re.sub("[\s0-9]", "", r)) > 0

            ## Checks for whether we are at a paragraph break,
            ## Defined as an empty line between two sections within an amendment
            if prior_not_empty and current_not_empty:
                r = r[amend_idx:-1]
                amend_text += r
                amend_text += ' '
            if not prior_not_empty and current_not_empty:
                r = r[amend_idx:-1]
                amend_text += r
                amend_text += ' '                
            elif prior_not_empty and not current_not_empty:
                amend_text = re.sub('^.*?Amendment\s', '', amend_text)
                if len(re.sub('[\s0-9]*', '', amend_text)) > len_threshold: 
                    this_amend['Paragraph ' + str(para_counter)] = amend_text
                    para_counter += 1
                    amend_text = ''
            elif not prior_not_empty and not current_not_empty:
                continue
            # if is_not_empty:
            #     r = r[amend_idx:-1]
            #     amend_text += r
            #     amend_text += ' '
        amendments_final.append(this_amend)

    amendment_labels = re.findall(amend_string, ''.join(doc))

    ## Get the amend1 locations:
    dup_index = []
    for i, a in enumerate(amendment_labels):
        if re.match('Amendment\s{1,}1{1}$', a):
            dup_index.append(i)
    print dup_index
    if len(dup_index) > 1:
        dup_index.pop(0)
    else:
        dup_index = None
    if dup_index:
        amend_labels = partition(amendment_labels, dup_index)
        amend_labels = dict(zip(committee_names, amend_labels))
    else:
        amend_labels = amendment_labels
        amend_labels = {committee_names[0]: amend_labels}

    
    out_labels = []
    for key in committee_names:
        for a in amend_labels[key]:
            out_labels.append([key, a]) ## fails here b/c index is wrong
            print a
    out_paras = []
    for element in amendments_final:
        this_para = [[k, v] for k, v in iter(sorted(element.iteritems()))]
        out_paras.append(this_para)

    out_list = []
    for l, m in zip(out_labels, out_paras):
        for p in m:
            lab = list(l)
            lab.extend(p)
            out_list.append(lab)

    #out = {'labels':amend_labels, 'amendments':amendments_final}

    return(out_list)

# conn = open('./rese/2007/test.out', 'rb')
# doc = [unicode(row, errors='replace') for row in conn]
# conn.close()

# clean_doc = [row.replace(u'\ufffd', ' ') for row in doc]

# test_parse = parse_w3m_output(clean_doc,
#                               clean_html=True,
#                               amend_idx=None,
#                               amend_string = 'Amendment\s*?[0-9]+?(?=\n)',
#                               end_string = 'Justification|PROCEDURE'
#                               )

# conn = open('./rese/2001/test.out', 'rb')
# doc = [unicode(row, errors='replace') for row in conn]
# conn.close()

# clean_doc = [row.replace(u'\ufffd', ' ') for row in doc]
# clean_doc = [re.sub('<.*?>', ' ', row) for row in clean_doc]

# test_parse_2 = parse_w3m_output(clean_doc,
#                                 amend_idx=39,
#                                 clean_html=False,
#                                 amend_string = 'Amendment.*?(?=\))',
#                                 end_string = 'Justification|PROCEDURE'
#                                 )



## parse_inline_amendments
## Takes as input the raw html record from, for instance, a parliamentary
## second reading. It assumes that amendments are not separate columns
## (as in first reading reports) but instead indicated by paragraphs with
## a specific form of highlighting. Parses and cleans those amendments and
## returns them as a list
## Inputs:
## raw_html: the raw html source from the EU Legislative Obs.
## amend_string: a regex string indicating the html tag that specifies
## the unique highlighting. For instace, <span.*?bold.*?>.
## Outputs:
## a list of amendments
def parse_inline_amendments(raw_html, section_string, amend_string = ''):

    paras = re.findall(section_string, raw_html, re.DOTALL)

    out = []
    for p in paras:
        if re.search(amend_string, p, re.DOTALL):
            p = re.sub('<.*?>', '', p)
            out.append(p)

    return(out)

# conn = open('./intl_mkt/2007/parl_second_reading.html', 'rb')
# doc = [unicode(row, errors='replace') for row in conn]
# conn.close()

# clean_doc = [row.replace(u'\ufffd', ' ') for row in doc]
# clean_doc = ''.join(clean_doc)

# test_parse_3 = parse_inline_amendments(clean_doc,
#                                        amend_string = '<span.*?italic.*?bold.*?>'
#                                        )


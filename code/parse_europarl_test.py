#from parse_europarl_amend_final import *
from parse_europarl_amend_final import *
import os
os.chdir('/Users/markhuberty/Documents/Research/Dissertation/master/notes/leg_hist_sources/')

## START TESTS

## Parse first reading report from 2003 
conn = open('./intl_mkt/2003/ep_first_reading_report.htm')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()



ep_2003_first_reading = parse_amend_html(input_string=input_string,
                                         re_tables=re_tables,
                                         re_clean=[re_clean_tables,
                                                   re_clean_table_tags,
                                                   re_span,
                                                   re_newline,
                                                   re_fn,
                                                   re_markup,
                                                   re_markup_links,
                                                   re_unicode_quote,
                                                   re_amp_quote,
                                                   re_semicolon,
                                                   re_endash]
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






conn = open('./intl_mkt/2007/ep_first_reading_report.htm', 'rb')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()




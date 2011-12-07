#from parse_europarl_amend_final import *
import os
os.chdir('/Users/markhuberty/Documents/Research/Papers/leghist/code')
from parse_europarl_amend_final import *
os.chdir('/Users/markhuberty/Documents/Research/Dissertation/master/notes/leg_hist_sources/')

parl_list = ['./intl_mkt/2003/ep_first_reading_report.htm',
             './intl_mkt/2003/ep_second_reading_report.htm',
             './intl_mkt/2007/ep_first_reading_report.htm']

committee_list = [['Industry, External Trade, Research, and Energy',
                   'Economic and Monetary Affairs',
                   'Legal Affairs and the Internal Market',
                   'Environment, Public Health, and Consumer Policy'],
                  ['Parliament, 2nd reading'],
                  ['Industry, External Trade, Research, and Energy',
                   'Economic and Monetary Affairs',
                   'Internal Market and Consumer Protection']
                  ]

bill_list = ['./intl_mkt/1996/directive_96_92_ec',
             './intl_mkt/2003/com_2001_0125_final_0077',
             './intl_mkt/2003/directive_2003_54_ec',
             './intl_mkt/2007/com_2007_528_final',
             './intl_mkt/2007/directive_2009_72_ec',
             './intl_mkt/2007/council_common_position'
             ]


preamble_string = 'HAVE ADOPTED THIS DIRECTIVE'
article_string = '\n\s*?Article\s[0-9]{1,}[ ]*\n'
#section_string = '\s*?[0-9]{1,}\.' ## might try to amend this to parse the (letter) form of lists
## This works and handles both major article sections and
## sub-lists as in (a), (b), (c) appropriately. This is now consistent with how
## the amendments come out of their parsing functions.
section_strings = ['\s+\n\s*?\({0,1}[a-z0-9]{1,3}[.\)]{1}\s', ## Works for html leg
                   '\n\s*?\({0,1}[a-z0-9]{1,3}[.\)]{1}\s' ## Works for council positions
                   ]
section_string_idx = [0,0,0,0,0,1]
                   
cleanup_strings = ['\n']
re_parl_amend_header = re.compile('Amendments by Parliament')

conn = open('./intl_mkt/2007/ep_draft_legislation_second_reading_source.html', 'rb')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

parl_second_reading = parse_legislation(input_string,
                                        preamble_string='<p.*?>HAVE ADOPTED THIS DIRECTIVE:</p>',
                                        article_string ='<p.*?>Article [0-9]*?</p>',
                                        section_string ='<p.*?>',
                                        cleanup_strings=['</{0,1}p.*?>',
                                                         '\\xa0|\\xc2',
                                                         '\n',
                                                         '</{0,1}span.*?>',
                                                         '</{0,1}a.*?>'
                                                         ]
                                        )

conn = open('./intl_mkt/2007/ep_draft_legislation_second_reading_source.txt', 'wb')
for a in parl_second_reading['articles']:
    for p in a:
        conn.write(p)
        conn.write('\n')
conn.close()



conn = open(parl_list[2], 'rb')
input_string = ''
for row in conn:
    input_string += (row + ' ')
conn.close()

## Messy; might be better after all to parse the html
## from the website if available. Idea:
## first split at the amendment + number tag (and do a findall to get the
## amendment labels directly); then
## split each chunk at "amendment" and take the second piece; then clean.
for c,p in zip(committee_list, parl_list):
    conn = open(p, 'rb')
    input_string = ''
    for row in conn:
        input_string += (row + ' ')
    conn.close()
    
    out = paired_amend_fun(input_string,
                           re_tables = re_tables,
                           re_amend_cleanup = [re_clean_tables,
                                               re_clean_table_tags,
                                               re_span,
                                               re_newline,
                                               re_fn,
                                               re_markup,
                                               re_markup_links,
                                               re_unicode_quote,
                                               re_amp_quote,
                                               re_semicolon,
                                               re_endash,
                                               re_parl_amend_header],
                           re_amend_header = re_amend,
                           re_amend_header_cleanup = [re_amend_clean, re_newline],
                           committee_names = c
                           )
    out_name = re.sub('htm', 'txt', p)

    conn = open(out_name, 'wb')
    writer = csv.writer(conn)
    writer.writerow(['committee', 'amendment', 'paragraph', 'text'])
    for item in out:
        writer.writerow(item)
    conn.close()
                     

for i, b in enumerate(bill_list):
    conn = open(b, 'rb')
    input_string = ''
    for row in conn:
        input_string += (row + ' ')

    section_string = section_strings[section_string_idx[i]]
    dict_out = parse_legislation(input_string,
                                 preamble_string,
                                 article_string,
                                 section_string,
                                 cleanup_strings
                                 )

    b_out = b + '.txt'

    conn = open(b_out, 'wb')
    for a in dict_out['articles']:
        for p in a:
            conn.write(p)
            conn.write('\n')
    conn.close()

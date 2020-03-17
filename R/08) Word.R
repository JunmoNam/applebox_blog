#==========================================================================
# Topic : Word
# Date : 2019. 04. 19
# Author : Junmo Nam
#==========================================================================


sapply(c('dplyr','docxtractr','XML'),require,character.only = T)

#read docx by line
word = read_docx('example.docx')
word

#==========================================================================
# 1.XML
#==========================================================================

#parsing
parsed = xmlParse(word$docx)

#get txt from xml data
xmlParse(word$docx) %>% getNodeSet('//w:t')
xmlParse(word$docx) %>% xpathSApply('//w:t',xmlValue)

#get txt by paragraph
xmlParse(word$docx) %>% xpathSApply('//w:p',xmlValue)


#check each paragraph and find style
paragraph = xmlParse(word$docx) %>% getNodeSet('//w:p')

#bold style
bold = paragraph[which(sapply(paragraph,function(x){saveXML(x) %>% grep('<w:b/>',.)})==1)] %>% sapply(xmlValue)

#italic style
itc = paragraph[which(sapply(paragraph,function(x){saveXML(x) %>% grep('<w:i/>',.)})==1)] %>% sapply(xmlValue)


#==========================================================================
# 2.Tables
#==========================================================================

#tables
tbl = docx_extract_all_tbls(word)
docx_extract_all_tbls(word,guess_header = F)


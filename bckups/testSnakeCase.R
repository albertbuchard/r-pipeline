string = "_a_b_c_abfg_b_c_aa_a"

gregexpr("_([a-z]{1})_([a-z]{1})_([a-z]{1})", text = string)

s1 = gsub("_([a-z]{1})_([a-z]{1})_([a-z]{1})_",  "_\\1\\2\\3_", string)
s2 = gsub("_([a-z]{1})_([a-z]{1})_",  "_\\1\\2_", s1)

string = "a__-___.___b"
string <- gsub("([-|.])", "_", string)
gsub("[_]{2,}", "_", string)
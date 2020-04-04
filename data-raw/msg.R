## code to prepare `DATASET` dataset goes here
setwd("/Users/phoebelam/Google Drive/chialam/stats resources/the kitchen/metachores")
# start of a function
imonit <- c("  m e t a c h o r e s  |  i'm on it. take an eye break, okie?",
            "  m e t a c h o r e s  |  i'm on it. have u been drinking water today?",
            "  m e t a c h o r e s  |  i'm on it. go enjoy a banana in bed",
            "  m e t a c h o r e s  |  i'm on it...while thinking of u ;)",
            "  m e t a c h o r e s  |  ur beauty is distracting me, go take a break!",
            "  m e t a c h o r e s  |  i'm on it. have u stretched today?",
            "  m e t a c h o r e s  |  i'm on it. stop staring at me. me shy. go take a break!")
usethis::use_data(imonit, internal = F, overwrite = T)

# end of a function
donzo <- c("  m e t a c h o r e s  |  donzo. have u taken eye breaks today yet?",
           "  m e t a c h o r e s  |  donzo. i'm charging one jessie smile.",
           "  m e t a c h o r e s  |  donzo. where's my reward? oh there u r ;)",
           "  m e t a c h o r e s  |  donzo. hey hey i just saved u potentially hours of work, spare minutes of those to take a break!",
           "  m e t a c h o r e s  |  donzo. u know i could have done it faster if u weren't staring at me this whole time.",
           "  m e t a c h o r e s  |  donzo. don't forget to take care of ur eye, okie?")
usethis::use_data(donzo, internal = F, overwrite = T)

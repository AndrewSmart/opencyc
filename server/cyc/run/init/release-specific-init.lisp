(in-package :cyc)

(ke-assert-now '(#$languageHasRootLexicon #$EnglishLanguage #$GeneralEnglishMt)
                #$GeneralLexiconMt)
(ke-assert-now '(#$languageCodeDigraph #$EnglishLanguage "en")
                #$CrossLinguisticLexicalMt)
(ke-assert-now '(#$validatedLexicalPSCForLanguage 
                 #$EnglishLanguage #$AllEnglishValidatedLexicalMicrotheoryPSC)
               #$LanguageAndWritingSystemMt)

(boolean (princ ";;  At this point the cyc http server is running and you can access
;;  Cyc directly via the local web browser.
;;  http://localhost:3602/cgi-bin/cyccgi/cg?cb-start
;;  You can browse cyc via the Guest account or perform updates by
;;  logging on as CycAdminstrator.
"))


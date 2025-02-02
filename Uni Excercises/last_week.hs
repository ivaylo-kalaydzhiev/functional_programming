import qualified Distribution.Compat.CharParsing as Data
-- 1. Безкрайни потоци
-- Не можем да ги конкатенираме

-- 2. Дървета, графи
-- "Път" е от корена до някъде, не задължително до листата

-- 3. Дълбоки списъци
-- Най-вероятно трябва на Scheme (останалите типове задачи на Haskell)

-- 4. Record & Tuples

-- База Данни - тип задача
-- 3 или 4 задачи за 3 часа


-- Зад 1
-- Даден е безкраен поток от естествени числа s. "Сегмент" на s наричаме
-- максимално дълга строго намаляваща последователсност от числа в s.
-- Всеки поток може да се разбие еднозначно на безкрайна редициа от
-- сегментите на даден поток s.

-- a) Да се реализира фунцкия segments, която връща поток от сегментите
-- на s
-- б) Да се реализира фунцкията fillSegments, която "попълва дупките"
-- в сегментите в потока s, така че всеки сегмент започващ с А да се замени
-- с сегмент с последователни числа от а до 0 

-- no functor, monads, operations - 

-- segments xs = segments_h $ zip xs (tail xs)
--     where
--         segments_h pairs = (map fst as) : (segments_h bs)
--             where (as, bs) = span (\(x, y) -> x > y) pairs


takeWhileDesc (x:xs@(y:_)) = if x > y then x : takeWhileDesc xs else [x]
dropWhileDesc (x:xs@(y:_)) = if x > y then dropWhileDesc xs else xs

segments [] = []
segments xs = (takeWhileDesc xs) : segments (dropWhileDesc xs)

-- >>> take 10 $ segments ([1, 5, 3, 2, 2, 6, 4, 7] ++ [1 ..])
-- [[1],[5,3,2],[2],[6,4],[7,1],[2],[3],[4],[5],[6]]

fillSegments xs = concatMap (\(x:_) -> [x, x-1 .. 0]) (segments xs)

-- >>> take 20 $ fillSegments ([1, 5, 3, 2, 2, 6, 4, 7] ++ [1 ..])
-- [1,0,5,4,3,2,1,0,2,1,0,6,5,4,3,2,1,0,7,6]

-- 3. Wordle
-- generate all n-char words and remove the ones for which these
-- filters apply

-- + - the char is in the correct place
-- ? - the char is not in teh correct place
-- - - the char does not exist in the word

-- wordle [("cat", "+-+"), ("use", "?--")]

-- shortWords = [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]

-- match :: String -> String -> [String] -> [String]
-- match [] [] cWords = cWords
-- match [] _  _      = []
-- match _  [] _      = []
-- match (w:ws) (p:ps) cWords
--     | p == '+' = match ws ps (filter (\word -> not (null word) && head word == w) cWords)
--     | p == '-' = match ws ps (filter (\word -> not (null word) && head word /= w) cWords)
--     | p == '?' = match ws ps (filter (\word -> elem w word) cWords)
--     | otherwise = error "Bad Error"

-- wordle :: [(String, String)] -> [[String]]
-- wordle [] = []
-- wordle ((word, pattern):xs) = (match word pattern shortWords) : wordle xs

-- >>> wordle [("cat", "+-+"), ("use", "?--")]
-- [[],["aau","abu","acu","adu","aeu","afu","agu","ahu","aiu","aju","aku","alu","amu","anu","aou","apu","aqu","aru","asu","atu","aua","aub","auc","aud","aue","auf","aug","auh","aui","auj","auk","aul","aum","aun","auo","aup","auq","aur","aus","aut","auu","auv","auw","aux","auy","auz","avu","awu","axu","ayu","azu","bau","bbu","bcu","bdu","beu","bfu","bgu","bhu","biu","bju","bku","blu","bmu","bnu","bou","bpu","bqu","bru","bsu","btu","bua","bub","buc","bud","bue","buf","bug","buh","bui","buj","buk","bul","bum","bun","buo","bup","buq","bur","bus","but","buu","buv","buw","bux","buy","buz","bvu","bwu","bxu","byu","bzu","cau","cbu","ccu","cdu","ceu","cfu","cgu","chu","ciu","cju","cku","clu","cmu","cnu","cou","cpu","cqu","cru","csu","ctu","cua","cub","cuc","cud","cue","cuf","cug","cuh","cui","cuj","cuk","cul","cum","cun","cuo","cup","cuq","cur","cus","cut","cuu","cuv","cuw","cux","cuy","cuz","cvu","cwu","cxu","cyu","czu","dau","dbu","dcu","ddu","deu","dfu","dgu","dhu","diu","dju","dku","dlu","dmu","dnu","dou","dpu","dqu","dru","dsu","dtu","dua","dub","duc","dud","due","duf","dug","duh","dui","duj","duk","dul","dum","dun","duo","dup","duq","dur","dus","dut","duu","duv","duw","dux","duy","duz","dvu","dwu","dxu","dyu","dzu","fau","fbu","fcu","fdu","feu","ffu","fgu","fhu","fiu","fju","fku","flu","fmu","fnu","fou","fpu","fqu","fru","fsu","ftu","fua","fub","fuc","fud","fue","fuf","fug","fuh","fui","fuj","fuk","ful","fum","fun","fuo","fup","fuq","fur","fus","fut","fuu","fuv","fuw","fux","fuy","fuz","fvu","fwu","fxu","fyu","fzu","gau","gbu","gcu","gdu","geu","gfu","ggu","ghu","giu","gju","gku","glu","gmu","gnu","gou","gpu","gqu","gru","gsu","gtu","gua","gub","guc","gud","gue","guf","gug","guh","gui","guj","guk","gul","gum","gun","guo","gup","guq","gur","gus","gut","guu","guv","guw","gux","guy","guz","gvu","gwu","gxu","gyu","gzu","hau","hbu","hcu","hdu","heu","hfu","hgu","hhu","hiu","hju","hku","hlu","hmu","hnu","hou","hpu","hqu","hru","hsu","htu","hua","hub","huc","hud","hue","huf","hug","huh","hui","huj","huk","hul","hum","hun","huo","hup","huq","hur","hus","hut","huu","huv","huw","hux","huy","huz","hvu","hwu","hxu","hyu","hzu","iau","ibu","icu","idu","ieu","ifu","igu","ihu","iiu","iju","iku","ilu","imu","inu","iou","ipu","iqu","iru","isu","itu","iua","iub","iuc","iud","iue","iuf","iug","iuh","iui","iuj","iuk","iul","ium","iun","iuo","iup","iuq","iur","ius","iut","iuu","iuv","iuw","iux","iuy","iuz","ivu","iwu","ixu","iyu","izu","jau","jbu","jcu","jdu","jeu","jfu","jgu","jhu","jiu","jju","jku","jlu","jmu","jnu","jou","jpu","jqu","jru","jsu","jtu","jua","jub","juc","jud","jue","juf","jug","juh","jui","juj","juk","jul","jum","jun","juo","jup","juq","jur","jus","jut","juu","juv","juw","jux","juy","juz","jvu","jwu","jxu","jyu","jzu","kau","kbu","kcu","kdu","keu","kfu","kgu","khu","kiu","kju","kku","klu","kmu","knu","kou","kpu","kqu","kru","ksu","ktu","kua","kub","kuc","kud","kue","kuf","kug","kuh","kui","kuj","kuk","kul","kum","kun","kuo","kup","kuq","kur","kus","kut","kuu","kuv","kuw","kux","kuy","kuz","kvu","kwu","kxu","kyu","kzu","lau","lbu","lcu","ldu","leu","lfu","lgu","lhu","liu","lju","lku","llu","lmu","lnu","lou","lpu","lqu","lru","lsu","ltu","lua","lub","luc","lud","lue","luf","lug","luh","lui","luj","luk","lul","lum","lun","luo","lup","luq","lur","lus","lut","luu","luv","luw","lux","luy","luz","lvu","lwu","lxu","lyu","lzu","mau","mbu","mcu","mdu","meu","mfu","mgu","mhu","miu","mju","mku","mlu","mmu","mnu","mou","mpu","mqu","mru","msu","mtu","mua","mub","muc","mud","mue","muf","mug","muh","mui","muj","muk","mul","mum","mun","muo","mup","muq","mur","mus","mut","muu","muv","muw","mux","muy","muz","mvu","mwu","mxu","myu","mzu","nau","nbu","ncu","ndu","neu","nfu","ngu","nhu","niu","nju","nku","nlu","nmu","nnu","nou","npu","nqu","nru","nsu","ntu","nua","nub","nuc","nud","nue","nuf","nug","nuh","nui","nuj","nuk","nul","num","nun","nuo","nup","nuq","nur","nus","nut","nuu","nuv","nuw","nux","nuy","nuz","nvu","nwu","nxu","nyu","nzu","oau","obu","ocu","odu","oeu","ofu","ogu","ohu","oiu","oju","oku","olu","omu","onu","oou","opu","oqu","oru","osu","otu","oua","oub","ouc","oud","oue","ouf","oug","ouh","oui","ouj","ouk","oul","oum","oun","ouo","oup","ouq","our","ous","out","ouu","ouv","ouw","oux","ouy","ouz","ovu","owu","oxu","oyu","ozu","pau","pbu","pcu","pdu","peu","pfu","pgu","phu","piu","pju","pku","plu","pmu","pnu","pou","ppu","pqu","pru","psu","ptu","pua","pub","puc","pud","pue","puf","pug","puh","pui","puj","puk","pul","pum","pun","puo","pup","puq","pur","pus","put","puu","puv","puw","pux","puy","puz","pvu","pwu","pxu","pyu","pzu","qau","qbu","qcu","qdu","qeu","qfu","qgu","qhu","qiu","qju","qku","qlu","qmu","qnu","qou","qpu","qqu","qru","qsu","qtu","qua","qub","quc","qud","que","quf","qug","quh","qui","quj","quk","qul","qum","qun","quo","qup","quq","qur","qus","qut","quu","quv","quw","qux","quy","quz","qvu","qwu","qxu","qyu","qzu","rau","rbu","rcu","rdu","reu","rfu","rgu","rhu","riu","rju","rku","rlu","rmu","rnu","rou","rpu","rqu","rru","rsu","rtu","rua","rub","ruc","rud","rue","ruf","rug","ruh","rui","ruj","ruk","rul","rum","run","ruo","rup","ruq","rur","rus","rut","ruu","ruv","ruw","rux","ruy","ruz","rvu","rwu","rxu","ryu","rzu","tau","tbu","tcu","tdu","teu","tfu","tgu","thu","tiu","tju","tku","tlu","tmu","tnu","tou","tpu","tqu","tru","tsu","ttu","tua","tub","tuc","tud","tue","tuf","tug","tuh","tui","tuj","tuk","tul","tum","tun","tuo","tup","tuq","tur","tus","tut","tuu","tuv","tuw","tux","tuy","tuz","tvu","twu","txu","tyu","tzu","uaa","uab","uac","uad","uae","uaf","uag","uah","uai","uaj","uak","ual","uam","uan","uao","uap","uaq","uar","uas","uat","uau","uav","uaw","uax","uay","uaz","uba","ubb","ubc","ubd","ube","ubf","ubg","ubh","ubi","ubj","ubk","ubl","ubm","ubn","ubo","ubp","ubq","ubr","ubs","ubt","ubu","ubv","ubw","ubx","uby","ubz","uca","ucb","ucc","ucd","uce","ucf","ucg","uch","uci","ucj","uck","ucl","ucm","ucn","uco","ucp","ucq","ucr","ucs","uct","ucu","ucv","ucw","ucx","ucy","ucz","uda","udb","udc","udd","ude","udf","udg","udh","udi","udj","udk","udl","udm","udn","udo","udp","udq","udr","uds","udt","udu","udv","udw","udx","udy","udz","uea","ueb","uec","ued","uee","uef","ueg","ueh","uei","uej","uek","uel","uem","uen","ueo","uep","ueq","uer","ues","uet","ueu","uev","uew","uex","uey","uez","ufa","ufb","ufc","ufd","ufe","uff","ufg","ufh","ufi","ufj","ufk","ufl","ufm","ufn","ufo","ufp","ufq","ufr","ufs","uft","ufu","ufv","ufw","ufx","ufy","ufz","uga","ugb","ugc","ugd","uge","ugf","ugg","ugh","ugi","ugj","ugk","ugl","ugm","ugn","ugo","ugp","ugq","ugr","ugs","ugt","ugu","ugv","ugw","ugx","ugy","ugz","uha","uhb","uhc","uhd","uhe","uhf","uhg","uhh","uhi","uhj","uhk","uhl","uhm","uhn","uho","uhp","uhq","uhr","uhs","uht","uhu","uhv","uhw","uhx","uhy","uhz","uia","uib","uic","uid","uie","uif","uig","uih","uii","uij","uik","uil","uim","uin","uio","uip","uiq","uir","uis","uit","uiu","uiv","uiw","uix","uiy","uiz","uja","ujb","ujc","ujd","uje","ujf","ujg","ujh","uji","ujj","ujk","ujl","ujm","ujn","ujo","ujp","ujq","ujr","ujs","ujt","uju","ujv","ujw","ujx","ujy","ujz","uka","ukb","ukc","ukd","uke","ukf","ukg","ukh","uki","ukj","ukk","ukl","ukm","ukn","uko","ukp","ukq","ukr","uks","ukt","uku","ukv","ukw","ukx","uky","ukz","ula","ulb","ulc","uld","ule","ulf","ulg","ulh","uli","ulj","ulk","ull","ulm","uln","ulo","ulp","ulq","ulr","uls","ult","ulu","ulv","ulw","ulx","uly","ulz","uma","umb","umc","umd","ume","umf","umg","umh","umi","umj","umk","uml","umm","umn","umo","ump","umq","umr","ums","umt","umu","umv","umw","umx","umy","umz","una","unb","unc","und","une","unf","ung","unh","uni","unj","unk","unl","unm","unn","uno","unp","unq","unr","uns","unt","unu","unv","unw","unx","uny","unz","uoa","uob","uoc","uod","uoe","uof","uog","uoh","uoi","uoj","uok","uol","uom","uon","uoo","uop","uoq","uor","uos","uot","uou","uov","uow","uox","uoy","uoz","upa","upb","upc","upd","upe","upf","upg","uph","upi","upj","upk","upl","upm","upn","upo","upp","upq","upr","ups","upt","upu","upv","upw","upx","upy","upz","uqa","uqb","uqc","uqd","uqe","uqf","uqg","uqh","uqi","uqj","uqk","uql","uqm","uqn","uqo","uqp","uqq","uqr","uqs","uqt","uqu","uqv","uqw","uqx","uqy","uqz","ura","urb","urc","urd","ure","urf","urg","urh","uri","urj","urk","url","urm","urn","uro","urp","urq","urr","urs","urt","uru","urv","urw","urx","ury","urz","usa","usb","usc","usd","use","usf","usg","ush","usi","usj","usk","usl","usm","usn","uso","usp","usq","usr","uss","ust","usu","usv","usw","usx","usy","usz","uta","utb","utc","utd","ute","utf","utg","uth","uti","utj","utk","utl","utm","utn","uto","utp","utq","utr","uts","utt","utu","utv","utw","utx","uty","utz","uua","uub","uuc","uud","uue","uuf","uug","uuh","uui","uuj","uuk","uul","uum","uun","uuo","uup","uuq","uur","uus","uut","uuu","uuv","uuw","uux","uuy","uuz","uva","uvb","uvc","uvd","uve","uvf","uvg","uvh","uvi","uvj","uvk","uvl","uvm","uvn","uvo","uvp","uvq","uvr","uvs","uvt","uvu","uvv","uvw","uvx","uvy","uvz","uwa","uwb","uwc","uwd","uwe","uwf","uwg","uwh","uwi","uwj","uwk","uwl","uwm","uwn","uwo","uwp","uwq","uwr","uws","uwt","uwu","uwv","uww","uwx","uwy","uwz","uxa","uxb","uxc","uxd","uxe","uxf","uxg","uxh","uxi","uxj","uxk","uxl","uxm","uxn","uxo","uxp","uxq","uxr","uxs","uxt","uxu","uxv","uxw","uxx","uxy","uxz","uya","uyb","uyc","uyd","uye","uyf","uyg","uyh","uyi","uyj","uyk","uyl","uym","uyn","uyo","uyp","uyq","uyr","uys","uyt","uyu","uyv","uyw","uyx","uyy","uyz","uza","uzb","uzc","uzd","uze","uzf","uzg","uzh","uzi","uzj","uzk","uzl","uzm","uzn","uzo","uzp","uzq","uzr","uzs","uzt","uzu","uzv","uzw","uzx","uzy","uzz","vau","vbu","vcu","vdu","veu","vfu","vgu","vhu","viu","vju","vku","vlu","vmu","vnu","vou","vpu","vqu","vru","vsu","vtu","vua","vub","vuc","vud","vue","vuf","vug","vuh","vui","vuj","vuk","vul","vum","vun","vuo","vup","vuq","vur","vus","vut","vuu","vuv","vuw","vux","vuy","vuz","vvu","vwu","vxu","vyu","vzu","wau","wbu","wcu","wdu","weu","wfu","wgu","whu","wiu","wju","wku","wlu","wmu","wnu","wou","wpu","wqu","wru","wsu","wtu","wua","wub","wuc","wud","wue","wuf","wug","wuh","wui","wuj","wuk","wul","wum","wun","wuo","wup","wuq","wur","wus","wut","wuu","wuv","wuw","wux","wuy","wuz","wvu","wwu","wxu","wyu","wzu","xau","xbu","xcu","xdu","xeu","xfu","xgu","xhu","xiu","xju","xku","xlu","xmu","xnu","xou","xpu","xqu","xru","xsu","xtu","xua","xub","xuc","xud","xue","xuf","xug","xuh","xui","xuj","xuk","xul","xum","xun","xuo","xup","xuq","xur","xus","xut","xuu","xuv","xuw","xux","xuy","xuz","xvu","xwu","xxu","xyu","xzu","yau","ybu","ycu","ydu","yeu","yfu","ygu","yhu","yiu","yju","yku","ylu","ymu","ynu","you","ypu","yqu","yru","ysu","ytu","yua","yub","yuc","yud","yue","yuf","yug","yuh","yui","yuj","yuk","yul","yum","yun","yuo","yup","yuq","yur","yus","yut","yuu","yuv","yuw","yux","yuy","yuz","yvu","ywu","yxu","yyu","yzu","zau","zbu","zcu","zdu","zeu","zfu","zgu","zhu","ziu","zju","zku","zlu","zmu","znu","zou","zpu","zqu","zru","zsu","ztu","zua","zub","zuc","zud","zue","zuf","zug","zuh","zui","zuj","zuk","zul","zum","zun","zuo","zup","zuq","zur","zus","zut","zuu","zuv","zuw","zux","zuy","zuz","zvu","zwu","zxu","zyu","zzu"]]


-- allWords 0 = [""]
-- allWords n = [c:w | c <- ['a' .. 'z'], w <- allWords (n - 1)]

-- isLetterCorrect word wordChar guessChar "+" = guessChar == wordChar
-- isLetterCorrect word wordChar guessChar "?" = guessChar `elem` word
-- isLetterCorrect word wordChar guessChar "-" = guessChar `notElem` word

-- isCorrect word guess result = and [isLetterCorrect word w g l | (w, g, l) <- zip3 word guess result]

-- wordle :: [(String, String)] -> String
-- wordle xs
--     | null matches = "no solution"
--     | length matches == 1 = head matches
--     | length matches > 1 = "many solutions"
--     where
--         n = length $ fst $ head xs
--         matches = [w | w <- allWords n, and [isCorrect w guess res | (guess, res) <- xs]]


-- >>> wordle [("cat", "+-+"), ("use", "?--")]
-- C:\Users\ivayl\Desktop\last_week.hs:94:64: error:
--     • Couldn't match type ‘Char’ with ‘[Char]’
--       Expected: [String]
--         Actual: String
--     • In the third argument of ‘isCorrect’, namely ‘res’
--       In the expression: isCorrect w guess res
--       In the first argument of ‘and’, namely
--         ‘[isCorrect w guess res | (guess, res) <- xs]’
-- (deferred type error)


-- [bad, '-?-'] -> defintely keep the +, move the 
-- []

-- Data.Char

-- cycle func 



type weekday = Mo | Tu | We | Th | Fr

type course = ALF | LIP

let isLecture day lesson = match (day, lesson) with
| (Tu, ALF) -> true
| (We, LIP) -> true
| (Th, _) -> true
| (Fr, ALF) -> true
| _ -> false;;
.onLoad <- function(...){
  # set some options here temporarily, remove them once the admins set them in opal:
  options('hidden.fields.regexes' = c('source'))
  options('hidden.fields' = c('person_id'))
  options('join.pivot.col' = c('person_id'))
  .init()
}

.onLoad <- function(...){
  # set some options here temporarily, remove them once the admins set them in opal:
  options('hidden.fields.regexes' = c('source'))
  options('hidden.fields' = c('person_id'))
  options('join.pivot.col' = c('person_id'))
  # and some more for datashield 6.2
  options('datashield.privacyControlLevel'  = 'permissive')
  options('default.nfilter.levels.density' = 0.33)
  options('default.nfilter.levels.max' = 40)
  .init()
}

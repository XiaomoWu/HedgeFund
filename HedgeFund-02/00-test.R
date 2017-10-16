ld(r.manager)
ld(r.manager.resume)
ld(r.manager.mapping)

manager <- r.manager[r.manager.resume, on = .(user_id), nomatch = 0]

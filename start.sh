exec erl -setcookie "123" -sname pw -pa planetwars/ebin/ _build/lib/*/ebin -config emu.config -s lager -eval "application:start(planetwars)"

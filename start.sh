exec erl -setcookie "123" -sname pw -pa planetwars/ebin/ _build/lib/*/ebin -config sys.config -s lager -eval "application:start(planetwars)"

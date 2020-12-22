CREATE TABLE users(user_id INTEGER PRIMARY KEY, display_name TEXT);
CREATE TABLE facebook_user_auths(facebook_id INTEGER PRIMARY KEY,user_id INTEGER);
CREATE TABLE favs(game_id INTEGER PRIMARY KEY,user_id INTEGER);
CREATE TABLE sessions(session_key TEXT PRIMARY KEY,user_id INTEGER);
CREATE TABLE IF NOT EXISTS "cache_games"(id INTEGER PRIMARY KEY,data TEXT);
CREATE TABLE cache_alternative_names(id INTEGER PRIMARY KEY,data TEXT);
CREATE TABLE user_profile(user_id INTEGER PRIMARY KEY,name TEXT);

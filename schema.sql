drop table if exists personalities;
create table personalities (
  p_id integer primary key autoincrement,
  name text not null
);

drop table if exists statements;
create table statements (
  statement_id integer primary key autoincrement,
  sdate date,
  p_id integer,
  text text not null,
  score_id integer,
);

drop table if exists scores;
create table scores (
  score_id integer primary key autoincrement,
  p_id integer,
);

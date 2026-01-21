sip_pingpong
=============

Домашнее задание №11
-----
Что было сделано:
----
- установлен twinkle;
- установлен docker;
- скопирован проект из лекции;
- собран образ с помощью скрипта ./docker-build-image.sh
- проверка добавления образа sudo docker image ls
- далее запуск контейнера ./docker-run-container.sh
- в конфиге твинкла изменен порт SIP на 5061, потому что работаю на VM.
- успешная регистрация:
```bash
5> sip_server: sip_authorize()
sip_server: trying to auth user []
sip_server: sip_authorize()
sip_server: trying to auth user []
sip_server: sip_authorize()
sip_server: trying to auth user []
sip_server: sip_get_user_pass(<<"3003">>)
sip_server: sip_authorize()
sip_server: trying to auth user []
sip_server: sip_route(User = <<>>)
sip_server: sip_register(From <<"3003">>)
REGISTER OK: {<<"3003">>,<<"localhost">>}
```
- далее тестовый звонок и обратный вызов через таймаут:
```bash
sip_server: sip_authorize()
sip_server: trying to auth user []
sip_server: sip_route(User = <<"3001">>)
sip_server: sip_invite(From <<"3003">>, User <<"3001">>)
sip_server: schedule callback to [{uri,sip,<<"3003">>,<<>>,<<"127.0.0.1">>,5061,<<>>,[],[],[],[],<<>>}]
sip_server: schedule_callback: wait timeout 10 sec...
sip_client: calling back to client [{uri,sip,<<"3003">>,<<>>,<<"127.0.0.1">>,5061,<<>>,[],[],[],[],<<>>}]
```
- номер сохраняется в таблице ets, управление в написанном модуле sip_client.db, создание таблицы в app;
- добавлен сервер cowboy, настраивается в app, обработка запросов в user_handler;
- звонок осуществляется по запросу /api/call/:userid, в случае, если id корректный, происходит звонок, если id не найден, то возвращаем ответ:
```bash
curl http://localhost:8080/api/call/123 
```
юзер с айди 123 отсутсвует в базе:
```bash
{"error":"UserId not found"}
```
Что можно улучшить
-----
- Покрыть тестами
- Использовать заголовочный файл с record контакта юзера
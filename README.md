# Flow Game

## Идея

RTS непрямого контролля про долглосрочное планирование.

### Предпосылки
Мне нравится RTS -- планировать игру, анализировать вражеские действия и подстраиваться под его поведение / навязывать ему своё. Однако на мой вкус микрить танчики это не барское занятие. Более того, микро танчиков оставляет не так много времени на ту часть с анализом и построением планов, т.к. зачастую является очень простым, понятным и эффективным способом размена времени и внимания на преимущество на поле.

### Что уже есть в мире?

Игра | Что нравится | Что не очень устраивает
----- | --------- | --------------
[Direct strike](https://starcraft.fandom.com/wiki/Direct_Strike) | Композиции и синергии юнитов, система автомикро с приоритетами, ощущение передавливания, тайминги | Привязка к SCII, отсутствие логистики, манёвров, борьбы за информацию, излишняя реактивность
[Supreme Commander](https://faforever.com/) | Системы экономики и производства, радиолектронная борьба, логистика, манёвры, система цепочек приказов, масштаб | Pathfinding, микро
[Netstorm](https://en.wikipedia.org/wiki/NetStorm:_Islands_At_War) | Позиционность, композиции и синергии башен | Тетрис

### Что в итоге хочется увидеть?

* Композиции и синергии юнитов
* Борьбу за информацию
* Масштаб, где на подготовку и исполнение планов нужно время
* Баланс между реактивностью и проактивностью
* Манёвры и логистику
* Время партии не больше полутра часов, лучше меньше

## Сеттинг
Война роботов с мертвяками

## Процесс

Игровой мир представляет из себя граф состоящий из больших и маленьких узлов. В начале игры у каждого игрока под контроллем есть по одному большому узлу. Цель: захватить стартовые узлы всех оппонирующих игроков. Для этого игроки захватывают и удерживают другие узлы, застраивают их, производят юниты и шлют их в атаку.

### Узлы
Существуют два вида узлов: большие и маленькие

* Большой узел. На нём есть слоты под экономические здания, оборонительные здания и под юниты. Возможно есть здания которые строятся на слотах для юнитов (?). Обычно служит базой.
* Маленький узел. На нём есть слоты только под оборонительные здания, и в меньшем количестве. Обычно служит перевалочным пунктом.

Для того чтобы захватить узел нужно чтобы войска добрались до него и сломали или захватили все постройки на нём. Застраивать можно только свои узлы.

### Производство и войска
Слоты под войска оформлены в виде сетки. На сетке размещаются конкретные юниты, которые и формируют очередь заказа. Узел по очереди пытается построить все юниты из очереди. Когда вся очередь готова, отряд выпускается и следует по кратчайшему пути до вражеского стартового узла. Промежуточные узлы пути можно менять. Если по пути есть союзный узел в котором идёт дозаказ, то отряд может подождать какое-то время (какое?) пока узел достроит свой отряд и дальше, слиться с ним и следовать дальше как один. Несформированный отряд и юниты других отрядов ожидающие формирование этого отряда участвуют в обороне узла. Если юниты из несформированного отряда гибнут, то узел пытается доукомплектовать отряд и не выпускает его, пока не укомплектует.

Каждый юнит отмечен одним или более тэгом, имеет скорость, здоровье и атаку (и может что-то ещё). Отряд всегда движется со скоростью равной минимальной скорости её участников. Когда отряд набредает на отряд противника просиходит симуляция боя (как?). Каждый убитый юнит оставляет после себя часть своих ресурсов в виде остова, которая затем может быть использована вновь.

Юнитам можно настраивать приоритеты атак в зависимости от тегов.

#### Симуляция боя
Бой предлагается симулировать высчитывая поведение для каждого юнита отдельно. У юнита есть направление куда он смотрит, скорость поворота. У способностей есть область рядом с юнитом откуда можно начать исполнение способности, зона поражения, а так же время на подготовку, время на исполнение, откат и перезарядка. Откат отличается от перезарядки тем, что блокирует исполнение всех связанных действий, а не только само себя.
Пример того как это может работать:
Допустим у нас есть юнит, который бьёт топором. Бьёт вот так (см картинку ниже). Эта область высчитывается относительно того куда смотрит юнит. У топора есть подготовка в 5 тиков, исполнение в 3 тика и откат в 5 тиков. В описаной выше системе возникают вот такие сценарии:
* Можно оббежать юнита с той стороны где у него нет топора. Тогда он не сможет ударить
* Можно подбежать и убить юнит до того как сработают 5 тиков подготовки удара
* Можно принять удар чем-то неважным и получить 10 тиков на убийство чем-то более мощным и ценным

В каждой клетке поля может находиться больше одного юнита одного вида (сколько именно для каждого вида юнитов своё). Юниты на одной клетке обсчитываются однотипно. Одна атака по умолчанию не может убить больше одного юнита, сколько бы урона она не нанесла, однако разные свойства атак могут это менять.

#### Полевые инженеры
В армию могут быть включены полевые инженеры. Они отличаются от обычных юнитов тем, что могут перерабатывать остовы в юнитов прямо на поле боя, а так же при захвате узла не разрушать, а захватывать постройки противника (как именно?)

#### Разведка
По умолчанию мы видим только то, что видят юниты. Однако в узлах можно строить радары. Они охватывают некоторую область вокруг себя и подсвечивают на ней юниты указанных тегов. Например радар подсвечивающий теги `A`и `B`, укажет что в области пробегает юнит с тегами `AB`, `A`, `B`. Однако юнит с тегом `AC` он распознает как `A`, а `CD` не увидит вовсе. У юнитов может быть стелс на некотором теге. Тогда юнит считается юнитом с этим тегом, но на радарах не подсвечивается (а может и подсвечивается, но на более сложных условиях).

#### Экономика
В целом экономические здания делятся на производящие и логистические. Производящие производят ресурс на склад узла где они построены. Логистические отправляют ресурсы со склада этого узла в другой узел или забирают ресурсы из некоторого узла или области и тащат в узел, где они построены. Производство юнитов всегда делается за счёт ресурсов на складе узла где они строятся. Потребление ресурсов (сколько максимум можно потратить ресурсов за тик) и производственная мощь (максимум юнитов, которое можно произвести за тик) так же регулируется зданиями в узле.

### Менее сформулированные идеи
* Авиация: транспорт создающий шорткаты, прикрытие
* Макро здания: дальнобойная артиллерия
* Крупные юниты
* Героические юниты

### Про фракции
#### Роботы
Большие, мощные, дорогие  юниты, которые долго делаются и которые можно настраивать. Однако у них заканчивается энергия, которая тратится на все действия робота. После того как энергия закончилась робот погибает.
Захваченные узлы соединяются энергосетью с соседними узлами, что повышает эффективность их работы. Чтобы получить доступ к лучшим источникам энергии нужно собирать особый ресурс нежити (см. ниже)

#### Нежить
Много юнитов, которые сами по себе не очень сильны, но практически бесплатны и более расположены к производству прямо на поля боя. Чтобы получить доступ к конкурентному вооружению должны разбирать на запчасти роботов, собирая их особый ресурс.
Особый ресурс нежити даётся по факту удержания узла и при смерти юнита автоматически, в полном обьёме возвращается в распоряжение нежити, если только его не захватят специальные юниты оппонента.

#### Как в среднем должно выглядеть их столкновение
Роботы идут огромными холмами, мертвяки бесчисленным потоком чёрти-кого, со вкраплениями больших чудовищ. Каждый выстрел робота прокладывает борозду в поле мертвяков, каждый взмах оружия расчищает сектор. От поверженной нечисти в воздух вылетают чёрные сгустки, часть из которых захватывают ловушки установленные на роботах. Эти ловушки подхватывают дроны и летят с ними на базу. Но если они будут делать это недостаточно эффективно, они завязнут, мертвяки из облепят и разберут на кусочки или большое чудовище разломает их напополам. И тогда у мертвяков появятся винтовки, пусть не такие разрушительные, но их будет очень много. Или даже собственные роботы, где дорогой металл будет заменён на кость, а недолговечные аккамуляторы бесконечным пыланием ада.

## Техническая часть
Всё планируется делать на Haskell + Reflex + SDL2. 
Причина: я люблю хаскелл, он позволяет писать очень выразительный код и много багов отлавливать на этапе компиляции. Рефлекс даёт довольно крутую абстракцию для фрп, а сдл проверенное временем решение для простой 2д графики + для рефлекса уже есть под него хост. 

Все данные планируется хранить вне кода. В идеале игра должна стать некоторым странным интерпретатором над внешними данными. Это даст возможность быстрой наработки контента как только проект получит хоть какую-то форму. Как формат внешних данных скорее всего будет использоваться dhall.

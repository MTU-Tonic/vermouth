# EAAI 2020 Gin Rummy Tournament Software
This project facilitates tournament evaluation and data collection for Gin Rummy player agents developed for the EAAI 2020 undergraduate research experience.

## Overview
The following examples **should** be runnable with the [release jar](https://github.com/MTU-Tonic/vermouth/releases) version of the tournament software, provided for your convenience. You should not need to compile the tournament software yourselves, except if you spot a bug or feature and would like to address it quicker than you think I can. Should you wish to compile the tournament software from source, you can use [Apache Maven](https://maven.apache.org/), which is a standard Java build system widely available. The command
```
mvn package
```
run at the repository's root should be sufficient to generate an executable JAR in the `target` directory. Please note that you should look for the generated JAR with `EXEC` in the file descriptor.

Agents can be compiled against the runtime software by adding `vermouth.jar` to the classpath when running `javac`. For example, on Unix and OSX
```
javac -cp ".:./vermouth.jar" MyGinRummyPlayer.java
```
and on Windows
```
javac -cp ".;./vermouth.jar" MyGinRummyPlayer.java
```
(note the colon vs the semicolon) will compile the file `MyGinRummyPlayer.java`, along side any other necessary files, using the vermouth package jar. The agent specified by the `MyGinRummyPlayer` class should be derived from `ginrummy.GinRummyPlayer` in order to work properly with this tournament software. Classes can exist within any package structure, however the following assumes that all classes reside in the default package.

As vermouth relies on the Todd Neller's [gin-rummy-eaai](https://github.com/tneller/gin-rummy-eaai) project to execute the game logic and understand the structure of agents, those classes are included in the jar and will be as up-to-date as the vermouth release. This eliminates the need for managing those files in your own project. Including the vermouth jar in your `javac` classpath will provide the `ginrummy` package and it's contents to the compiler.

Optionally, you can bundle the resulting class files into a JAR or alternatively just ZIP your class files. The tournament software supports both deployments. __TODO:__ At this point, it is untested whether or not this approach will allow the resulting agents to load external resources from their respective deployment directory/file. However, it is noted that such an act is common and support will be pursued in the near future.

### Examples
These examples are provided for unix/bsd-based operating systems (including OSX). **Windows users note:** it has been reported that you must use full absolute paths to be properly recognized as a URL, e.g., ```"file://D:/vermouth-master/players/mtu/AI_20"```.

1) For two Gin Rummy agents, `MyGinRummyPlayer.class` and `YourGinRummyPlayer.class`, both in the current directory, execute a one-vs-one competition consisting of 100 games.
```
java -jar vermouth.jar 
   --oneall 
   --games 100 
   --agents 'file:./MyGinRummyPlayer' 'file:./YourGinRummyPlayer'
```
Results are stored in a timestamped zip file with no game or agent output.

2) For four Gin Rummy agents, each named `AwesomeAgent.class` but independently stored in unique JAR files `Competitor0.jar`, `Competitor1.jar`, `Competitor2.jar`, and `Competitor3.jar`, all in the current directory, execute a round-robin competition consisting of 50 games for each match. 
```
java -jar vermouth.jar 
   --roundrobin 
   --games 50
   --agents 'jar:file:./Competitor0.jar!/AwesomeAgent' 
            'jar:file:./Competitor1.jar!/AwesomeAgent' 
            'jar:file:./Competitor2.jar!/AwesomeAgent' 
            'jar:file:./Competitor3.jar!/AwesomeAgent'
   --verbose
```
Results are stored in a timestamped zip file and all game and agent output is cataloged.

3) One Gin Rummy agent, `MyGinRummyPlayer.class`, is run against three Gin Rummy agents, each named `Agent.class` but independently stored in unique JAR files `SimpleApproach.jar`, `CounterfactualApproach.jar`, and `AdversarialApproach.jar`, each in the directory `/home/jshiebel/agents/`, with each match consisting of 1000 games.
```
java -jar vermouth.jar 
   --elo 
   --games 50
   --agents 'file:./MyGinRummyPlayer' 
            'jar:file:./SimpleApproach.jar!/AwesomeAgent' 
            'jar:file:./CounterfactualApproach.jar!/AwesomeAgent'
            'jar:file:./AdversarialApproach.jar!/AwesomeAgent' 
   --verbose
   --identifier "MyGinRummyPlayer-ComparisonTest"
```
Results are stored in a zip file named `MyGinRummyPlayer-ComparisonTest.zip` and all game and agent output is cataloged.

## Usage
The project will load class files which are derived from `eaai.ginrummy.game.GinRummyPlayer`. In order to compile agents against the provided contracts, you should run `javac` with `vermouth.jar` included on the classpath, e.g.,
```
javac -cp ".:./vermouth.jar" MyGinRummyPlayer.java
```
The result will generate `MyGinRummyPlayer.class`, along with any other required class files.

Agents are loaded at runtime using Java reflection on specified class files. The following details the command-line arguments.
```
usage: java -jar vermouth.jar
 -1v,--oneall            run a series of 1v1 competitions between the
                         first competitor and subsequent competitors
 -rr,--roundrobin        run a round robin competition between the
                         competitors
 -el,--elo               run a Elo-scored competition between the
                         competitors
 -g,--games <games>      number of rounds to evaluate per match
 -a,--agents <agents>    agent classfile specifications
 -id,--identifier <id>   run identifier, used as filename prefix, with
                         timestamp default 
 -s,--seed <s>           random seed (hexidecimal) for generating game
                         seeds
 -v,--verbose            output game details
 -l,--log <level>        the logging level [OFF, FATAL, ERROR, WARN, INFO,
                         DEBUG, TRACE, ALL]
 -h,--help               print this description
```

Either a set of 1v1 competitions (`-1v,--oneall`), a full round robin competition (`-rr,--roundrobin`), or an Elo-based ranking competitiion (`-el,--elo`) is run. Each match within the tournament faces off two of the specified agents (`--agents <agents>`) for a total of rounds (`--games <games>`). The result is a win percentage for different agent pairings for the specified tournament setting. 

### Tournament Type (`-1v,--oneall` or `-rr,--roundrobin` or `-el,--elo`)
There are three different types of tournaments supported by this software. The first, `--oneall`, specifies a one-vs-all competition should be held between the first listed agent and the subsequent listed agents. The second, `--roundrobin`, specifies a round-robin competition wherein each listed agent competes with other listed agents. Agents can be specified multiple times, with each being loaded and initialized independently. The third, `--elo`, specifies an Elo ranking-based tournament which evaluates agents in a round-robin fashion and updates agent Elo rankings in a batch.

### Parameters (`-g,--games <games>`)
Each tournament type specifies a sequence of matches which is determined by the number of participating players and the tournament type. Each of those matches will consist of a number of games (`--games`). All matches will have an identical number of games.

### Competitor Agents (`-a,--agents <agents>`)
Each agent is specified as a URL with a designated protocol. 
- For class files which are to be __loaded directly from the file system__, you must use `file:` as a protocol, e.g., `file:./SimpleGinRummyPlayer`. In this case, the tournament software will load the file `SimpleGinRummyPlayer.class` (assumed to be derived from `eaai.ginrummy.game.GinRummyPlayer`) in the current relative directory. Note the absence of the `.class` suffix. 
- For class files wich are to be __loaded from a jar (or a zip)__, you must use `jar:file:` as a protocol (it is still `jar:file:` even if the file is just a plain zip containing your code), e.g.,  `jar:file:./SimpleGinRummyPlayer.jar!/SimpleGinRummyPlayer`. In this case, the tournament software will load the file `SimpleGinRummyPlayer.class` contained in the jar `SimpleGinRummyPlayer.jar` (assumed to be derived from `eaai.ginrummy.game.GinRummyPlayer`) in the current relative directory. Note the absence of the `.class` suffix.

Any number of agents can be specified. Each agent will be numbered according to the order of agent specified for the purposes of identifying game results.

### Identifier (`-id,--identifier <id>`)
The identifier for the run, `--identifier`, specifies the string designation for this match, and determines the output filename. See __Output__ below for more details.

### Player File I/O
Developers may wish to use external data files which store utility information, either to provide parameterized data which influences a player's decision making (e.g., loading policy information generated by external data analysis, machine learning, etc.) or to store game data and event occurrences for later analysis. The primary challenge for offering file support include providing suitable file locations and accidental overlap in file utilization between competitors.

The core concept taken by Vermouth is to place files near code. More specifically, files will be referenced relative to the directory containing the __base package__. Each player, when loaded at runtime using reflection, is associated with a classloader. That classloader is based at the root of the project directory. To illustrate, consider the following directory structure:
```
~/Workspace/ginrummy-tournament/michigantech/
  events.txt
  stats.txt
  mtu/
    DataKeeping.java (mtu.DataKeeping)
    Utility.java (mtu.Utility)
      agents/
        SimplePlayer.java (mtu.agents.SimplePlayer)
    policy.dat
```
So for the `GinRummyPlayer` derivative `mtu.agents.SimplePlayer` is located at directory `~/Workspace/ginrummy-tournament/michigantech/mtu/agents/SimplePlayer.class` then the classloader is associated with the directory `~/Workspace/ginrummy-tournament/michigantech`. This will be the same regardless of the location of the vermouth tournament system.

In order to ensure that there are no surprises with file I/O when developing and transmitting players, a file reference feature has been added to `GinRummyUtil`. Each `GinRummyUtil.FileReference` object is associated with a `Class<?>` object and a string representing the relative path to the file of interest. Relating this back to our `mtu.agents.SimplePlayer` example, the `FileReference` created with
```
GinRummyUtil.FileReference stats = new GinRummyUtil.FileReference(MTUPlayer_20.class, "stats.dat")
```
would be searching for the file located at `~/Workspace/vermouth/players/stats.dat`. Every class in your code will result in the same location, so any class can be provided as the first location. With a `FileReference` created, there are three methods available to you:
```
boolean GinRummyUtil.FileReference.exists()
OutputStream GinRummyUtil.FileReference.asOutputStream(OpenOption...)
InputStream GinRummyUtil.FileReference.asInputStream(OpenOption...)
```
That is, `exists()`, `asOutputStream(OpenOption...)` (to write) and `asInputStream(OpenOption...)` (to read). In the case of writing and reading files, the `OutputStream` and `InputStream` objects returned should easily allow for use in other I/O classes such as `PrintWriter` and `Scanner`. The `OpenOption` variable arguments are as per `StandardOpenOption` which includes enumeration values such as `CREATE` and `APPEND`.

Example usage can be found in the `SimpleFileGinRummyPlayer` available in the `eaai-gin-rummy` package.

**Note:** At this time, the system will not create new directories under any circumstances! This inconvenience doubles as an accidental security measure. If you wish to use a folder for files, be sure that folder is created as a part of your submission!

## Output
Other than some nomial terminal output for the purpose of detailing tournament progress, all tournament data is generated and stored in a zip file generated by the specified identifier (`--identifier <id>`). If no such identifier is specified, a timestamp is generated. All output from contestants and tournament software is saved to files within the zip file. This includes:
- `players.csv`: Stores the agent id (by index) and the associated agent class URL. The data is formatted as a CSV which is ammenable to loading and data processing in common languages (python/R). This is mostly for review or collating results. It can be used to translate the corresponding player ids (0-indexed based on command-line argument specification) in other files to human-readable specifications.
- `rounds.csv`: Elo ranking information at specific rounds of tournament progress.
- `games.csv`: Stores the unique match id, participating agents (by index), game number within the match, final game score, and winning player. The data is formatted as a CSV which is ammenable to loading and data processing in common languages (python/R).
- `stats.csv`: Stores runtime statistics of different agent calls. Currently this will report the runtime (in nanoseconds) of every `GinRummyPlayer` function call, along with the corresponding agent (by index). __TODO__ Include memory utilization.
- `M-XvY-gameG.txt`: Stores tournament software output for each game (`M` for the unique match id, `X` and `Y` for the agents participating, and `G` for the game number within a match). The output catalogs a human-readable log of the game state, actions taken, and any agent output. Can be enabled or disabled by the verbose flage (`--verbose`).

The level of termainal output can also be specified (using `--log <level>`), but this is mostly just a debugging reference for myself. The default level is set to INFO, which should provide sufficient information while still informing the user of any exceptions which may be encountered.

## Scripts
The output has been selected to represent a breadth of information that can be analysed and used by external tools. To illustrate the workflow one could use to interact with the generated archive files, please refer to the `scripts` directory.



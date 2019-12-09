import sbtassembly.AssemblyPlugin.defaultShellScript

test in assembly := {}
assemblyJarName in assembly := "mal.jar"
mainClass in assembly := Some("Step0_repl")
assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

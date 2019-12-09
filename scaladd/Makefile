TARGET_DIR=target/scala-2.13

all: $(TARGET_DIR)/mal.jar

dist: mal

mal: $(TARGET_DIR)/mal.jar
	cp $< $@

$(TARGET_DIR)/mal.jar:
	sbt assembly

$(TARGET_DIR)/classes/step%.class:
	sbt assembly

clean:
	rm -rf mal target

.PHONY: all dist clean

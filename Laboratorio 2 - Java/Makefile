JAVAC = javac
JAVA = java
SRC_DIR = src
LIB_DIR = lib
LIBS = $(wildcard $(LIB_DIR)/*.jar)
CLASSPATH = $(SRC_DIR):$(subst $(SPACE),:,$(LIBS))
JAVAC_FLAGS = -d bin -cp $(CLASSPATH)
JAVA_FILES = $(shell find $(SRC_DIR) -name "*.java")
CLASS_FILES = $(patsubst $(SRC_DIR)/%.java,bin/%.class,$(JAVA_FILES))
MAIN_CLASS = App
OUTPUT_DIR = bin

SPACE := $(empty) $(empty)

all: $(CLASS_FILES)

bin/%.class: src/%.java
	$(JAVAC) $(JAVAC_FLAGS) $<

run: bin/$(MAIN_CLASS).class
	@$(JAVA) -cp $(CLASSPATH):bin $(MAIN_CLASS) $(ARGS)

clean:
	rm -rf $(OUTPUT_DIR)/*.class

.PHONY: all run clean

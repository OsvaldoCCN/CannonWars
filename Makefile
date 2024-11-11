# Define las carpetas de salida para los archivos .o y el ejecutable
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = bin
EXECUTABLE = $(BIN_DIR)/navalStrike

# Lista de fuentes en la carpeta src
SOURCES = $(SRC_DIR)/main.hs  # Añade aquí otros archivos .hs en src si es necesario

# Crea las carpetas necesarias y compila el proyecto
all: $(BUILD_DIR) $(BIN_DIR) $(EXECUTABLE)

# Crea la carpeta build si no existe
$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Crea la carpeta bin si no existe
$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# Compila el proyecto y guarda los .o en la carpeta build y el ejecutable en bin
$(EXECUTABLE): $(SOURCES) | $(BUILD_DIR) $(BIN_DIR)
	ghc -i$(SRC_DIR) -outputdir $(BUILD_DIR) -o $(EXECUTABLE) $(SOURCES)

# Limpia los archivos generados (.hi, .o, y el ejecutable)
clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR) *.hi

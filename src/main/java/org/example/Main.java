package org.example;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvException;
import net.sf.clipsrules.jni.*;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        // Crear instancia de RecomendacionSBR
        RecomendacionSBR sistemaRecomendacion = new RecomendacionSBR();

        try {
            // Cargar reglas desde un archivo CLIPS
            sistemaRecomendacion.cargarReglas("Data/reglas-optativa-java.clp");

            // Leer datos de playas desde un archivo o fuente de datos
            List<Playa> playas = leerDatosPlayas();

            // Ejecutar el sistema de recomendación
            sistemaRecomendacion.ejecutarSistemaRecomendacion(playas);
        } catch (CLIPSException e) {
            e.printStackTrace();
        }
    }

    private static List<Playa> leerDatosPlayas() {
        String filePath = "Data/2_PraiasBandeiraAzul_2023.csv";

        List<Playa> playas = new ArrayList<>();

        // 1. Leer datos de CSV y crear instancias de Playa
        try (CSVReader csvReader = new CSVReaderBuilder(new FileReader(filePath)).build()) {
            // Leer todas las filas del CSV
            List<String[]> data = csvReader.readAll();
            int len = 114;

            // Iterar sobre las filas (omitir la primera fila con títulos)
            for (int i = 1; i < len; i++) {
                String col[] = data.get(i)[0].split(";");

                // Crear instancia de Playa y asignar valores
                Playa playa = new Playa(col);

                // Agregar la instancia de Playa a la lista
                playas.add(playa);
            }
        } catch (IOException | CsvException e) {
            e.printStackTrace();
        }
        return playas;
    }
}

package org.example;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvException;
import net.sf.clipsrules.jni.CLIPSException;
import net.sf.clipsrules.jni.Environment;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Historial {
    private static Environment clips;

    public static void main(String[] args) {
        // Crear instancia de RecomendacionSBR
        clips = new Environment();

        try {
            // Cargar reglas desde un archivo CLIPS
            clips.load("Data/r-hist-java.clp");

            // Leer datos de playas desde un archivo o fuente de datos
            List<Playa> playas = leerDatosPlayas();

            // Ejecutar el sistema de recomendación
            clips.reset();

            // Preparar datos de playas para insertar en CLIPS
            PrepararDatosPlayas(playas);

            // Preparar filtro
            String selecNombre = "A Praia das Catedrais";
            String selecRespuesta = "c";
            clips.assertString(String.format("(PlayaHistorial (playa_nombre \"%s\") (respuesta %s) (fin nil) (playa-r))",
                    selecNombre, selecRespuesta));
            clips.run();


        } catch (CLIPSException e) {
            e.printStackTrace();
        }
    }

    public static void PrepararDatosPlayas(List<Playa> playas) throws CLIPSException {
        // Iterar sobre la lista de playas
        for (Playa playa : playas) {
            try {
                String assertCommand = String.format(
                        "(Playa (provincia \"%s\") (concello \"%s\") (nombre \"%s\") (lugar-parroquia \"%s\") (longitud \"%s\") " +
                                "(tipo \"%s\") (tipo-arena \"%s\"))",
                        playa.getProvincia(), playa.getConcello(), playa.getNombre(), playa.getLugarParroquia(),
                        playa.getLongitud(), playa.getTipo(), playa.getTipoArea()
                );
                //System.out.println(assertCommand);
                clips.assertString(assertCommand);

            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
                System.out.println("Al insertar la playa: " + playa.getNombre());
            }
        }
    }

    private static List<Playa> leerDatosPlayas() {
        String filePath = "Data/BH_Historial_Todas.csv";

        List<Playa> playas = new ArrayList<>();

        // 1. Leer datos de CSV y crear instancias de Playa
        try (CSVReader csvReader = new CSVReaderBuilder(new FileReader(filePath)).build()) {
            // Leer todas las filas del CSV
            List<String[]> data = csvReader.readAll();
            int len = 113;

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

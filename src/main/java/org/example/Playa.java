package org.example;

public class Playa {
    private String provincia;
    private String codigoProvincia;
    private String concello;
    private String codigoConcello;
    private String nombre;
    private String lugarParroquia;
    private String longitud;
    private String tipo;
    private String tipoArea;
    private String coordenadas1;
    private String coordenadas2;


    // Constructor, getters y setters
    public Playa(String provincia, String codigoProvincia, String concello, String codigoConcello, String nombre, String lugarParroquia, String longitud, String tipo, String tipoArea, String coordenadas1, String coordenadas2) {
        this.provincia = provincia;
        this.codigoProvincia = codigoProvincia;
        this.concello = concello;
        this.codigoConcello = codigoConcello;
        this.nombre = nombre;
        this.lugarParroquia = lugarParroquia;
        this.longitud = longitud;
        this.tipo = tipo;
        this.tipoArea = tipoArea;
        this.coordenadas1 = coordenadas1;
        this.coordenadas2 = coordenadas2;
    }

    public Playa(String[] col) {
        this.provincia = col[0];
        this.codigoProvincia = col[1];
        this.concello = col[2];
        this.codigoConcello = col[3];
        this.nombre = col[4];
        this.lugarParroquia = col[5];
        this.longitud = col[6];
        this.tipo = col[7];
        this.tipoArea = col[8];
        this.coordenadas1 = col[9];
        this.coordenadas2 = col[10];
    }

    public Playa() {
        this.provincia = "";
        this.codigoProvincia = "";
        this.concello = "";
        this.codigoConcello = "";
        this.nombre = "";
        this.lugarParroquia = "";
        this.longitud = "";
        this.tipo = "";
        this.tipoArea = "";
        this.coordenadas1 = "";
        this.coordenadas2 = "";
    }

    public String getProvincia() {
        return provincia;
    }

    public void setProvincia(String provincia) {
        this.provincia = provincia;
    }

    public String getCodigoProvincia() {
        return codigoProvincia;
    }

    public void setCodigoProvincia(String codigoProvincia) {
        this.codigoProvincia = codigoProvincia;
    }

    public String getConcello() {
        return concello;
    }

    public void setConcello(String concello) {
        this.concello = concello;
    }

    public String getCodigoConcello() {
        return codigoConcello;
    }

    public void setCodigoConcello(String codigoConcello) {
        this.codigoConcello = codigoConcello;
    }

    public String getNombre() {
        return nombre;
    }

    public void setNombre(String nombre) {
        this.nombre = nombre;
    }

    public String getLugarParroquia() {
        return lugarParroquia;
    }

    public void setLugarParroquia(String lugarParroquia) {
        this.lugarParroquia = lugarParroquia;
    }

    public String getLongitud() {
        return longitud;
    }

    public void setLongitud(String longitud) {
        this.longitud = longitud;
    }

    public String getTipo() {
        return tipo;
    }

    public void setTipo(String tipo) {
        this.tipo = tipo;
    }

    public String getTipoArea() {
        return tipoArea;
    }

    public void setTipoArea(String tipoArea) {
        this.tipoArea = tipoArea;
    }

    public String getCoordenadas1() {
        return coordenadas1;
    }

    public void setCoordenadas1(String coordenadas1) {
        this.coordenadas1 = coordenadas1;
    }

    public String getCoordenadas2() {
        return coordenadas2;
    }

    public void setCoordenadas2(String coordenadas2) {
        this.coordenadas2 = coordenadas2;
    }

    @Override
    public String toString() {
        return "Playa{" +
                "provincia='" + provincia + '\'' +
                ", codigoProvincia='" + codigoProvincia + '\'' +
                ", concello='" + concello + '\'' +
                ", codigoConcello='" + codigoConcello + '\'' +
                ", nombre='" + nombre + '\'' +
                ", lugarParroquia='" + lugarParroquia + '\'' +
                ", longitud='" + longitud + '\'' +
                ", tipo='" + tipo + '\'' +
                ", tipoArea='" + tipoArea + '\'' +
                ", coordenadas='(" + coordenadas1 + ", " + coordenadas2 + ")" + '\'' +
                '}';
    }
}

package org.example;

public class Playa {
    private String provincia;
    private String concello;
    private String nombre;
    private String lugarParroquia;
    private String longitud;
    private String tipo;
    private String tipoArea;
    private Integer poboacion;


    // Constructor, getters y setters


    public Playa(String provincia, String concello, String nombre, String lugarParroquia, String longitud, String tipo, String tipoArea, Integer poboacion) {
        this.provincia = provincia;
        this.concello = concello;
        this.nombre = nombre;
        this.lugarParroquia = lugarParroquia;
        this.longitud = longitud;
        this.tipo = tipo;
        this.tipoArea = tipoArea;
        this.poboacion = poboacion;
    }

    public Playa(String[] col) {
        this.provincia = col[0];
        this.concello = col[1];
        this.nombre = col[2];
        this.lugarParroquia = col[3];
        this.longitud = col[4];
        this.tipo = col[5];
        this.tipoArea = col[6];
        this.poboacion = Integer.parseInt(col[7]);
    }

    public Playa() {
        this.provincia = "";
        this.concello = "";
        this.nombre = "";
        this.lugarParroquia = "";
        this.longitud = "";
        this.tipo = "";
        this.tipoArea = "";
        this.poboacion = 0;
    }

    public String getProvincia() {
        return provincia;
    }

    public void setProvincia(String provincia) {
        this.provincia = provincia;
    }

    public String getConcello() {
        return concello;
    }

    public void setConcello(String concello) {
        this.concello = concello;
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

    public Integer getPoboacion() {
        return poboacion;
    }

    public void setPoboacion(Integer poboacion) {
        this.poboacion = poboacion;
    }

    @Override
    public String toString() {
        return "Playa{" +
                "provincia='" + provincia + '\'' +
                ", concello='" + concello + '\'' +
                ", nombre='" + nombre + '\'' +
                ", lugarParroquia='" + lugarParroquia + '\'' +
                ", longitud='" + longitud + '\'' +
                ", tipo='" + tipo + '\'' +
                ", tipoArea='" + tipoArea + '\'' +
                ", poboacion='(" + poboacion + ")" + '\'' +
                '}';
    }
}

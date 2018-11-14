def add_point_clusters(mapobj, df, popup_field_list):
    # Function to add point clusters (from http://andrewgaidus.com/leaflet_webmaps_python/)
    # Would require careful editing to match our points

    # Create empty lists to contain the point coordinates and the point pop-up information
    coords, popups = [], []
    # Loop through each record in the GeoDataFrame
    for i, row in df.iterrows():
        # Append lat and long coordinates to "coords" list
        coords.append([row.geometry.y, row.geometry.x])

        # Create a string of HTML code used in the IFrame popup
        # Join together the fields in "popup_field_list" with a linebreak between them
        label = '<br>'.join([row[field] for field in popup_field_list])
        # Append an IFrame that uses the HTML string to the "popups" list
        popups.append(IFrame(label, width=300, height=100))

    # Create a Folium feature group for this layer, since we will be displaying multiple layers
    pt_lyr = folium.FeatureGroup(name='pt_lyr')

    # Add the clustered points of crime locations and popups to this layer
    pt_lyr.add_children(MarkerCluster(locations=coords, popups=popups))

    # Add this point layer to the map object
    mapobj.add_children(pt_lyr)
    return mapobj

boston = add_point_clusters(boston, list_clust, ['Descript', 'Address', 'DateStr', 'Time'])
Phase I clinical trials typically focus on Safety Analysis and are usually of short duration. Researchers, such as data managers and analysts, often need to promptly grasp the status of data. However, the process from SDTM to report can still be time-consuming. Recognizing this need, this app is designed to streamline this process and provide immediate insights.

### This app aims to:
- Allow users to upload various types of data independently from local.
- Provide straightforward data insight features. With this app, users can quickly generate data insights, helping them to understand and interpret their data more effectively.

This app is designed to empower users, providing them with the tools they need to effectively manage and understand their data in Phase I clinical trials. By reducing wait times and simplifying data analysis, we hope to make the process of clinical trials more efficient and productive.

### Problems solved:
Based on teal_data_module, implemented dynamic data upload, supports multiple formats of data files("*.csv, *.xlsx, *.xpt, and *.sas7bdat");
Developed a function to reprocess data when importing CDISC datasets from external sources to **comply with the valid S4 object** required by teal.modules.clinical, such as adding join_keys() from metadata.

### Future Goals:
- Perform more comprehensive processing on datasets uploaded from local sources to adapt them for `teal.modules.clinical`.
- Add more summary modules;
- Enable user interaction, allowing for datapoint highlighting;
- Incorporate a Patient Profile feature.
- Enhance page UI design

Thanks to the teal team for their great work and for inspiring my ongoing exploration and learning in this field. The core functionalities of this app are all developed based on the teal package; I merely learned how to use this package.

Notes:
- If you want to load `teal.modules.clinical`, please use valid CDISC datasets. I have prepared some example datasets in *xpt, *sas7bdat formats for you to experience this app.
- The initial loading of this app may take approximately 5-10 seconds. The speed of data upload depends on the file size. Once the data is successfully uploaded, the response time for data operations within the app is very fast, thanks to the teal package.
- This app does not store the uploaded data, but it is not recommended to upload sensitive project data.

![BriskViewer_Demonstration_01Nov2024](https://github.com/user-attachments/assets/233e18c6-cf66-43ed-bebb-784cd969b2a5)

[Vedio Recording of Demonstration]( https://drive.google.com/file/d/1p2xS4vLFOQ5JZF8zFA2NBX9JwgyHSPoC/view?usp=sharing)





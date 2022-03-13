import tkinter as tk


def configure(window, row, column):
    window.rowconfigure(row, weight=1)
    window.columnconfigure(column, weight=1)

def populate_patient_info(window, row):
    patient_info_frame = tk.Frame( master=window, borderwidth=2)
    patient_info_frame.grid(row=row, column=0)
    patient_age_frame = tk.Frame( master=patient_info_frame, borderwidth=2)
    patient_age_frame.grid(row=row, column=0)
    configure(window, row, 0)

    patient_age_label = tk.Label(master=patient_age_frame, text='Patient Age (mo)')
    patient_age_label.pack()

    patient_age_entry = tk.Entry(master=patient_age_frame)
    patient_age_entry.pack()

    device_info_frame = tk.Frame( master=patient_info_frame, borderwidth=2)
    device_info_frame.grid(row=row, column=1)

    device_info_label = tk.Label(master=device_info_frame, borderwidth=2, text='Medical Device')
    device_info_label.pack()

    device_info_var = tk.BooleanVar()
    yes_btn = tk.Radiobutton(master=device_info_frame, text='Yes', variable=device_info_var, value=True)
    yes_btn.pack()
    no_btn = tk.Radiobutton(master=device_info_frame, text='No', variable=device_info_var, value=False)
    no_btn.pack()



def populate_cardio_info(window, row):
    cardio_frame = tk.Frame(master=window, borderwidth=2)
    cardio_frame.grid(row=row, column=0)

    hr_frame = tk.Frame(master=cardio_frame, borderwidth=2)
    hr_frame.grid(row=row, column=0)

    hr_label = tk.Label(master=hr_frame, text='Heart Rate (bpm)')
    hr_label.pack()

    hr_entry = tk.Entry(master=hr_frame)
    hr_entry.pack()

    bpsys_frame = tk.Frame(master=cardio_frame, borderwidth=2)
    bpsys_frame.grid(row=row, column=1)

    bpsys_label = tk.Label(master=bpsys_frame, text='BP (Sys)')
    bpsys_label.pack()

    bpsys_entry = tk.Entry(master=bpsys_frame)
    bpsys_entry.pack()


def setup_frontend():

    row = 0
    window = tk.Tk()

    label = tk.Label( master=window
                    , text="Pediatric Sepsis Screen Tool"
                    , height=3)

    label.grid(row=row, column=0)
    row = row + 1

    # Frames
    populate_patient_info(window, row)
    row = row + 1
    populate_cardio_info(window, row)


    #respiratory_frame = tk.Frame(master=window, borderwidth=2)



    return window


if __name__ == '__main__':
    window = setup_frontend()

    try:
        window.mainloop()
    except KeyboardInterrupt:
        print('Bye')





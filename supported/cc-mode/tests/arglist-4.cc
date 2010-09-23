main()
{
    process_manager->UpdateProcess( &ms_obj,
                                    server_addr );

    process_manager->UpdateProcess( ms_obj,
                                    server_addr );
}
